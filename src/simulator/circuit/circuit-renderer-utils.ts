/* eslint-disable no-console */
import { Position } from "../common";
import { Circuit } from ".";

// TODO: Test coverage
export const entityHasConnectedWires = (
  pins: string[],
  wires: string[][]
): boolean =>
  pins.some((pin) => wires.some((wire) => wire.some((id) => id === pin)));

export const computeReferencePoint = (
  waypoint: Position,
  lastWaypoint: Position,
  curveFactor: number = 0.8
): Position => ({
  x: lastWaypoint.x + curveFactor * (waypoint.x - lastWaypoint.x),
  y: lastWaypoint.y + curveFactor * (waypoint.y - lastWaypoint.y),
});

export const renderSummary = (circuit: Circuit) => {
  console.log("\n");
  console.log(`| CIRCUIT: ${circuit.name} |`);
  for (const input of circuit.entities.inputs) {
    console.log(`input: ${input.id} (${input.pin.id})`);
  }
  for (const output of circuit.entities.outputs) {
    console.log(`output: ${output.id} (${output.pin.id})`);
  }
  for (const chip of circuit.entities.chips) {
    console.log(`chip: ${chip.name} (${chip.id})`);
    for (const inputPin of chip.inputPins) {
      console.log(`-- pin: ${inputPin.id} (${inputPin.chip.name})`);
    }
    for (const outputPin of chip.outputPins) {
      console.log(`-- pin: ${outputPin.id} (${outputPin.chip.name})`);
    }
  }
  for (const wire of circuit.entities.wires) {
    console.log(
      `wire: ${wire.startPin.id} (${wire.startPin.chip.name}) -> ${wire.endPin.id} (${wire.endPin.chip.name})`
    );
  }
};
