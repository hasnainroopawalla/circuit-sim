import { CoreChip, CoreGate, CustomChip, IOChip } from "../chip";
import { Circuit } from "../circuit";
import { CustomChipBlueprint, CustomChipSchema } from "../circuit.interface";
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
      current: {
        inputs: newInputs,
        outputs: newOutputs,
        chips: newChips,
        wires: newWires,
      },
    };
  }

  public static blueprintToCircuit(
    p: p5,
    id: string,
    name: string,
    color: string,
    rawCircuit: CustomChipSchema,
    blueprint: CustomChipBlueprint
  ): CustomChip {
    console.log("Chip name:", name, rawCircuit);

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
      let createdChip: CustomChip | CoreChip;
      if (["AND", "OR", "NOT"].includes(chip.name)) {
        createdChip = new CoreChip(p, chip.name as CoreGate, chip.id);
      } else {
        createdChip = this.blueprintToCircuit(
          p,
          chip.id,
          chip.name,
          color,
          blueprint[chip.name],
          blueprint
        );
      }

      console.log("pushing chip to circuit", chip.name, createdChip);
      circuit.chips.push(createdChip);
    }

    // CircuitHelper.renderSummary(circuit);
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

    const customChip = new CustomChip(p, circuit, id, "green");

    // console.log("RETURN CUSTOM CHIP", customChip.name);
    CircuitHelper.renderSummary(customChip.circuit);
    return customChip;
  }

  public static blueprintToCustomChip(
    p: p5,
    blueprint: CustomChipBlueprint,
    name: string,
    color: string
  ): CustomChip {
    const customChip = this.blueprintToCircuit(
      p,
      "ID",
      name,
      color,
      blueprint["current"],
      blueprint
    );
    console.log("FINAL", customChip);

    return customChip;
  }
}
