import { CustomChip, IOChip, CoreChip, CoreGate } from "../../chip";
import { Circuit } from "../circuit";
import { CustomChipBlueprint, CustomChipSchema } from "../circuit.interface";

export const circuitToBlueprint = (
  name: string,
  circuit: Circuit,
  blueprint: CustomChipBlueprint = {}
): CustomChipBlueprint => {
  const newInputs = circuit.entities.inputs.map((input) => ({
    id: input.id,
  }));

  const newOutputs = circuit.entities.outputs.map((output) => ({
    id: output.id,
  }));

  const newChips: CustomChipSchema["chips"] = [];
  for (const chip of circuit.entities.chips) {
    if (chip instanceof CustomChip) {
      circuitToBlueprint(chip.name, chip.circuit, blueprint);
    }
    newChips.push({
      id: chip.id,
      name: chip.name,
    });
  }

  const newWires = circuit.entities.wires.map((wire) => {
    const wireStart = `${wire.startPin.chip.id}/${
      wire.startPin.isInput ? "input" : "output"
    }.${wire.startPin.id}`;
    const wireEnd = `${wire.endPin.chip.id}/${
      wire.endPin.isInput ? "input" : "output"
    }.${wire.endPin.id}`;
    return [wireStart, wireEnd];
  });

  blueprint[name] = {
    inputs: newInputs,
    outputs: newOutputs,
    chips: newChips,
    wires: newWires,
  };

  return blueprint;
};

export const blueprintToCircuit = (
  p: p5,
  name: string,
  blueprintString: string,
  defaultCircuitName?: string // "main"
): Circuit => {
  const blueprint: CustomChipBlueprint = JSON.parse(blueprintString);
  const circuitSchema = defaultCircuitName
    ? blueprint[defaultCircuitName]
    : blueprint[name];

  // an object to map the blueprint entity id to the actual entity created by the circuit
  // this is required since the circuit is fully responsible for instantiating the entities
  const entities: { [id: string]: IOChip | CustomChip | CoreChip } = {};

  const circuit = new Circuit(
    p,
    name,
    {
      position: {
        x: 0,
        y: 0,
      },
      size: {
        w: 0,
        h: 0,
      },
    },
    true
  );

  for (const input of circuitSchema.inputs) {
    entities[input.id] = circuit.createIOChip("input");
  }

  for (const output of circuitSchema.outputs) {
    entities[output.id] = circuit.createIOChip("output");
  }

  for (const chip of circuitSchema.chips) {
    const createdChip = ["AND", "OR", "NOT"].includes(chip.name)
      ? circuit.createCoreChip(chip.name as CoreGate)
      : circuit.createCustomChip(
          blueprintToCircuit(p, chip.name, blueprintString),
          "green" // fixed color of all internal chips
        );

    entities[chip.id] = createdChip;
  }

  for (const wire of circuitSchema.wires) {
    const {
      chipId: startChipId,
      pinType: startPinType,
      pinId: startPinId,
    } = parseWireString(wire[0], entities);
    const {
      chipId: endChipId,
      pinType: endPinType,
      pinId: endPinId,
    } = parseWireString(wire[1], entities);

    const startPin = entities[startChipId].getPin(startPinType, startPinId);
    const endPin = entities[endChipId].getPin(endPinType, endPinId);

    if (startPin && endPin) {
      circuit.spawnWire(startPin, endPin);
    }
  }

  return circuit;
};

// TODO: tests
const parseWireString = (
  wireString: string,
  entities: { [id: string]: IOChip | CustomChip | CoreChip }
): {
  chipId: string;
  pinType: string;
  pinId: number;
} => {
  const [chipId, pinInfo] = wireString.split("/");
  const splitChipId = chipId.split(".");
  const splitPinInfo = pinInfo.split(".");

  const [parsedChipId, pinType, pinId] =
    splitChipId.length === 5 && !entities[chipId]
      ? [splitChipId.slice(0, 3).join("."), splitChipId[3], splitChipId[4]]
      : [chipId, splitPinInfo[0], splitPinInfo[1]];

  return {
    chipId: parsedChipId,
    pinType,
    pinId: Number(pinId),
  };
};
