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
    const newInputs = circuit.inputs.map((input) => ({
      id: input.id,
    }));
    // TODO: add the filters
    // .filter((element) =>
    //   CircuitHelper.entityHasConnectedWires([element.pin], newWires)
    // );

    const newOutputs = circuit.outputs.map((output) => ({
      id: output.id,
    }));
    // .filter((entity) =>
    //   CircuitHelper.entityHasConnectedWires([entity.pin], newWires)
    // );

    const newChips: CustomChipSchema["chips"] = [];
    for (let i = 0; i < circuit.chips.length; i++) {
      const chip = circuit.chips[i];
      const createdChip = {
        id: chip.id,
        name: chip.name,
      };
      newChips.push(createdChip);
      if (chip instanceof CustomChip) {
        this.circuitToBlueprint(chip.name, chip.circuit, blueprint);
      }
    }

    const newWires = circuit.wires.map((wire) => [
      `${wire.startPin.chip.id}-${wire.startPin.id}`,
      `${wire.endPin.chip.id}-${wire.endPin.id}`,
    ]);

    blueprint[name] = {
      inputs: newInputs,
      outputs: newOutputs,
      chips: newChips,
      wires: newWires,
    };

    return blueprint;
  }

  private static parseWireString(wireString: string): {
    chipId: string;
    pinId: string;
  } {
    const splitString = wireString.split("-");
    return {
      chipId: splitString[0],
      pinId: splitString[1],
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
    console.log(name);

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

    console.log(entities);

    for (let i = 0; i < circuitSchema.wires.length; i++) {
      const wire = circuitSchema.wires[i];
      console.log("WIRE", wire);
      const { chipId: startChipId, pinId: startPinId } = this.parseWireString(
        wire[0]
      );
      const { chipId: endChipId, pinId: endPinId } = this.parseWireString(
        wire[1]
      );

      console.log(endChipId, endPinId);

      const startPin = entities[startChipId].getPin(startPinId);
      const endPin = entities[endChipId].getPin(endPinId);
      if (startPin && endPin) {
        circuit.spawnWire(startPin, endPin);
      }
    }

    const customChip = new CustomChip(p, circuit, id, color);

    CircuitHelper.renderSummary(customChip.circuit);
    return customChip;
  }
}
