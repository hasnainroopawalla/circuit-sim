import { Circuit } from "../circuit";
import { Position } from "../shared.interface";

// TODO: Test coverage
export class CircuitHelper {
  public static entityHasConnectedWires(
    pins: string[],
    wires: string[][]
  ): boolean {
    return pins.some((pin) => {
      return wires.some((wire) => {
        return wire.some((id) => id === pin);
      });
    });
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
    for (let i = 0; i < circuit.inputs.length; i++) {
      const input = circuit.inputs[i];
      console.log(`input: ${input.id} (${input.pin.id})`);
    }
    for (let i = 0; i < circuit.outputs.length; i++) {
      const output = circuit.outputs[i];
      console.log(`output: ${output.id} (${output.pin.id})`);
    }
    for (let i = 0; i < circuit.chips.length; i++) {
      const chip = circuit.chips[i];
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
    for (let i = 0; i < circuit.wires.length; i++) {
      const wire = circuit.wires[i];
      console.log(
        `wire: ${wire.startPin.id} (${wire.startPin.chip.name}) -> ${wire.endPin.id} (${wire.endPin.chip.name})`
      );
    }
  }
}
