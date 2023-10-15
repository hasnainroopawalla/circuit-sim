import { CoreChip, CoreGate, CustomChip, IOChip } from "../chip";
import { Circuit } from "../circuit";
import type {
  CustomChipBlueprint,
  CustomChipSchema,
} from "../circuit.interface";
import CircuitHelper from "./circuitHelper";

// TODO: Test coverage
export default class BlueprintHelper {
  public static circuitToBlueprint(
    name: Circuit["name"],
    wires: Circuit["wires"],
    inputs: Circuit["inputs"],
    outputs: Circuit["outputs"],
    chips: Circuit["chips"]
  ): CustomChipBlueprint {
    const newWires = wires.map((wire) => [wire.startPin.id, wire.endPin.id]);

    const newInputs = inputs
      .map((input) => ({
        id: input.id,
        pin: input.pin.id,
      }))
      .filter((element) =>
        CircuitHelper.entityHasConnectedWires([element.pin], newWires)
      );

    const newOutputs = outputs
      .map((output) => ({
        id: output.id,
        pin: output.pin.id,
      }))
      .filter((entity) =>
        CircuitHelper.entityHasConnectedWires([entity.pin], newWires)
      );

    const newChips = chips
      .map((chip) => ({
        id: chip.id,
        name: chip.name,
        inputPins: chip.inputPins.map((pin) => pin.id),
        outputPins: chip.outputPins.map((pin) => pin.id),
      }))
      .filter((entity) =>
        CircuitHelper.entityHasConnectedWires(
          [...entity.inputPins, ...entity.outputPins],
          newWires
        )
      );

    return {
      main: {
        inputs: newInputs,
        outputs: newOutputs,
        chips: newChips,
        wires: newWires,
      },
    };
  }

  public static blueprintToCustomChip(
    p: p5,
    id: string,
    name: string,
    color: string,
    rawCircuit: CustomChipSchema,
    blueprint: CustomChipBlueprint
  ): CustomChip {
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

    for (let i = 0; i < rawCircuit.inputs.length; i++) {
      const input = rawCircuit.inputs[i];
      circuit.inputs.push(
        new IOChip(p, input.id, true, {
          x: 0,
          y: 0,
        })
      );
    }

    for (let i = 0; i < rawCircuit.outputs.length; i++) {
      const output = rawCircuit.outputs[i];
      circuit.outputs.push(
        new IOChip(p, output.id, false, {
          x: 0,
          y: 0,
        })
      );
    }

    for (let i = 0; i < rawCircuit.chips.length; i++) {
      const chip = rawCircuit.chips[i];

      const createdChip = ["AND", "OR", "NOT"].includes(chip.name)
        ? new CoreChip(p, chip.name as CoreGate, chip.id)
        : this.blueprintToCustomChip(
            p,
            chip.id,
            chip.name,
            color,
            blueprint[chip.name],
            blueprint
          );

      circuit.chips.push(createdChip);
    }

    for (let i = 0; i < rawCircuit.wires.length; i++) {
      const wire = rawCircuit.wires[i];
      const [startPin, endPin] = [
        circuit.getPinById(wire[0]),
        circuit.getPinById(wire[1]),
      ];
      if (startPin && endPin) {
        circuit.spawnWire(startPin, endPin);
      }
    }

    return new CustomChip(p, circuit, id, color);
  }
}
