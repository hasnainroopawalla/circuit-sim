/* eslint-disable no-console */
import { Circuit } from "../circuit";
import { Position } from "../shared.interface";

// TODO: Test coverage
export class CircuitHelper {
  public static entityHasConnectedWires(
    pins: string[],
    wires: string[][]
  ): boolean {
    return pins.some((pin) =>
      wires.some((wire) => wire.some((id) => id === pin))
    );
  }

  public static computeReferencePoint(
    waypoint: Position,
    lastWaypoint: Position,
    curveFactor: number = 0.8
  ): Position {
    return {
      x: lastWaypoint.x + curveFactor * (waypoint.x - lastWaypoint.x),
      y: lastWaypoint.y + curveFactor * (waypoint.y - lastWaypoint.y),
    };
  }

  public static renderSummary(circuit: Circuit) {
    console.log("\n");
    console.log(`| CIRCUIT: ${circuit.name} |`);
    for (let i = 0; i < circuit.entities.inputs.length; i++) {
      const input = circuit.entities.inputs[i];
      console.log(`input: ${input.id} (${input.pin.id})`);
    }
    for (let i = 0; i < circuit.entities.outputs.length; i++) {
      const output = circuit.entities.outputs[i];
      console.log(`output: ${output.id} (${output.pin.id})`);
    }
    for (let i = 0; i < circuit.entities.chips.length; i++) {
      const chip = circuit.entities.chips[i];
      console.log(`chip: ${chip.name} (${chip.id})`);
      for (let i = 0; i < chip.inputPins.length; i++) {
        const inputPin = chip.inputPins[i];
        console.log(`-- pin: ${inputPin.id} (${inputPin.chip.name})`);
      }
      for (let i = 0; i < chip.outputPins.length; i++) {
        const outputPin = chip.outputPins[i];
        console.log(`-- pin: ${outputPin.id} (${outputPin.chip.name})`);
      }
    }
    for (let i = 0; i < circuit.entities.wires.length; i++) {
      const wire = circuit.entities.wires[i];
      console.log(
        `wire: ${wire.startPin.id} (${wire.startPin.chip.name}) -> ${wire.endPin.id} (${wire.endPin.chip.name})`
      );
    }
  }
}
