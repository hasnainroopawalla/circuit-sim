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
    name: string,
    circuit: Circuit,
    blueprint: CustomChipBlueprint = {}
  ): CustomChipBlueprint {
    CircuitHelper.renderSummary(circuit);

    const newWires = circuit.wires.map((wire) => [
      wire.startPin.id,
      wire.endPin.id,
    ]);

    const newInputs = circuit.inputs.map((input) => ({
      id: input.id,
      pin: input.pin.id,
    }));
    // .filter((element) =>
    //   CircuitHelper.entityHasConnectedWires([element.pin], newWires)
    // );

    const newOutputs = circuit.outputs.map((output) => ({
      id: output.id,
      pin: output.pin.id,
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
        inputPins: chip.inputPins.map((pin) => pin.id),
        outputPins: chip.outputPins.map((pin) => pin.id),
      };
      console.log("CREATED CHIP", createdChip);
      newChips.push(createdChip);
      if (chip instanceof CustomChip) {
        this.circuitToBlueprint(chip.name, chip.circuit, blueprint);
      }
    }

    console.log("BEFORE", blueprint);

    blueprint[name] = {
      inputs: newInputs,
      outputs: newOutputs,
      chips: newChips,
      wires: newWires,
    };

    console.log("AFTER", blueprint);
    // const newChips = circuit.chips
    //   .map((chip) => ({
    //     id: chip.id,
    //     name: chip.name,
    //     inputPins: chip.inputPins.map((pin) => pin.id),
    //     outputPins: chip.outputPins.map((pin) => pin.id),
    //   }))
    //   .filter((entity) =>
    //     CircuitHelper.entityHasConnectedWires(
    //       [...entity.inputPins, ...entity.outputPins],
    //       newWires
    //     )
    //   );

    return blueprint;
  }

  public static blueprintToCustomChip(
    p: p5,
    id: string,
    name: string,
    color: string,
    circuitSchema: CustomChipSchema,
    blueprint: CustomChipBlueprint
  ): CustomChip {
    // console.log(name, circuitSchema);
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
      const createdInput = new IOChip(p, input.id, true, {
        x: 0,
        y: 0,
      });
      // circuit.spawnInputIOChip();
      circuit.inputs.push(createdInput);
    }
    // console.log(circuit.inputs.length, circuit.inputs[0]);

    for (let i = 0; i < circuitSchema.outputs.length; i++) {
      const output = circuitSchema.outputs[i];
      circuit.outputs.push(
        new IOChip(p, output.id, false, {
          x: 0,
          y: 0,
        })
      );
    }

    for (let i = 0; i < circuitSchema.chips.length; i++) {
      const chip = circuitSchema.chips[i];
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

    for (let i = 0; i < circuitSchema.wires.length; i++) {
      const wire = circuitSchema.wires[i];
      const [startPin, endPin] = [
        circuit.getPinById(wire[0]),
        circuit.getPinById(wire[1]),
      ];
      if (startPin && endPin) {
        circuit.spawnWire(startPin, endPin);
      }
    }
    const cc = new CustomChip(p, circuit, id, color);
    // console.log(cc);
    return cc;
  }
}
