import { CoreChip, CoreGate, CustomChip, IOChip } from "../chip";
import { Circuit } from "../circuit";
import type {
  CustomChipBlueprint,
  CustomChipSchema,
} from "../circuit.interface";
import CircuitHelper from "./circuitHelper";

export default class BlueprintHelper {
  public static circuitToBlueprint(
    name: string,
    circuit: Circuit,
    blueprint: CustomChipBlueprint = {}
  ): CustomChipBlueprint {
    // TODO: convert pin id to index based on input/output index of parent chip
    const newWires = circuit.wires.map((wire) => {
      wire.startPin.isInput;
      const wireStart = `${wire.startPin.chip.id}/${
        wire.startPin.isInput ? "input" : "output"
      }.${wire.startPin.id}`;
      const wireEnd = `${wire.endPin.chip.id}/${
        wire.endPin.isInput ? "input" : "output"
      }.${wire.endPin.id}`;
      return [wireStart, wireEnd];
    });

    const newInputs = circuit.inputs.map((input) => ({
      id: input.id,
    }));
    // TODO: add the filters
    // .filter((element) =>
    //   CircuitHelper.entityHasConnectedWires([element.pin], newWires)
    // );

    // console.log("inputs", newInputs);
    const newOutputs = circuit.outputs.map((output) => ({
      id: output.id,
    }));
    // .filter((entity) =>
    //   CircuitHelper.entityHasConnectedWires([entity.pin], newWires)
    // );
    // console.log("outputs", newOutputs);

    const newChips: CustomChipSchema["chips"] = [];
    for (let i = 0; i < circuit.chips.length; i++) {
      const chip = circuit.chips[i];
      if (chip instanceof CustomChip) {
        this.circuitToBlueprint(chip.name, chip.circuit, blueprint);
      }
      // console.log("chip - ", chip);
      const createdChip = {
        id: chip.id,
        name: chip.name,
      };
      newChips.push(createdChip);
    }
    // console.log("chips", newChips);

    blueprint[name] = {
      inputs: newInputs,
      outputs: newOutputs,
      chips: newChips,
      wires: newWires,
    };

    return blueprint;
  }

  // TODO: check
  private static parseWireString(
    wireString: string,
    entities: { [id: string]: IOChip | CustomChip | CoreChip }
  ): {
    chipId: string;
    pinType: string;
    pinId: number;
  } {
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
  }

  public static blueprintToCustomChip(
    p: p5,
    id: string,
    name: string,
    color: string,
    circuitSchema: CustomChipSchema,
    blueprint: CustomChipBlueprint
  ): CustomChip {
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

    for (let i = 0; i < circuitSchema.inputs.length; i++) {
      const input = circuitSchema.inputs[i];
      entities[input.id] = circuit.spawnInputIOChip();
    }

    for (let i = 0; i < circuitSchema.outputs.length; i++) {
      const output = circuitSchema.outputs[i];
      entities[output.id] = circuit.spawnOutputIOChip();
    }

    for (let i = 0; i < circuitSchema.chips.length; i++) {
      const chip = circuitSchema.chips[i];

      const createdChip = ["AND", "OR", "NOT"].includes(chip.name)
        ? circuit.createCoreChip(chip.name as CoreGate)
        : this.blueprintToCustomChip(
            p,
            chip.id,
            chip.name,
            color,
            blueprint[chip.name],
            blueprint
          );
      entities[chip.id] = createdChip;
    }

    for (let i = 0; i < circuitSchema.wires.length; i++) {
      const wire = circuitSchema.wires[i];
      const {
        chipId: startChipId,
        pinType: startPinType,
        pinId: startPinId,
      } = this.parseWireString(wire[0], entities);
      const {
        chipId: endChipId,
        pinType: endPinType,
        pinId: endPinId,
      } = this.parseWireString(wire[1], entities);

      const startPin = entities[startChipId].getPin(startPinType, startPinId);
      const endPin = entities[endChipId].getPin(endPinType, endPinId);

      if (startPin && endPin) {
        circuit.spawnWire(startPin, endPin);
      }
    }

    const customChip = new CustomChip(p, circuit, id, color);

    return customChip;
  }
}
