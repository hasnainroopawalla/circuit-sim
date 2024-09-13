import type { Position } from "../../../common";
import type { ICircuitBoard } from "../../circuit-board.interface";

/**
 * Computes a reference point for the wire's waypoint marker.
 */
export const computeReferencePoint = (
  waypoint: Position,
  lastWaypoint: Position,
  curveFactor: number = 0.8
): Position => ({
  x: lastWaypoint.x + curveFactor * (waypoint.x - lastWaypoint.x),
  y: lastWaypoint.y + curveFactor * (waypoint.y - lastWaypoint.y),
});

/**
 * Only used for debugging.
 */
export const renderSummary = (circuitBoard: ICircuitBoard) => {
  console.log("\n");
  console.log(`| CIRCUIT: ${circuitBoard.name} |`);
  for (const input of circuitBoard.entities.inputs) {
    console.log(`input: ${input.id} (${input.pin.id})`);
  }
  for (const output of circuitBoard.entities.outputs) {
    console.log(`output: ${output.id} (${output.pin.id})`);
  }
  for (const chip of circuitBoard.entities.chips) {
    console.log(`chip: ${chip.name} (${chip.id})`);
    for (const inputPin of chip.inputPins) {
      console.log(`-- pin: ${inputPin.id} (${inputPin.chip.name})`);
    }
    for (const outputPin of chip.outputPins) {
      console.log(`-- pin: ${outputPin.id} (${outputPin.chip.name})`);
    }
  }
  for (const wire of circuitBoard.entities.wires) {
    console.log(
      `wire: ${wire.startPin.id} (${wire.startPin.chip.name}) -> ${wire.endPin.id} (${wire.endPin.chip.name})`
    );
  }
};
