import { CoreGate } from "../chip";
import { Circuit } from "../circuit";
import { CustomChipBlueprint } from "../circuit.interface";
import { Position } from "../shared.interface";

// TODO: Test coverage
export default class CircuitHelper {
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
        coreGate: chip.name as CoreGate,
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
      name,
      inputs: newInputs,
      outputs: newOutputs,
      chips: newChips,
      wires: newWires,
    };
  }

  public static computeReferencePoint(
    waypoint: Position,
    lastWaypoint: Position,
    curveFactor: number = 0.9
  ): Position {
    const referencePoint = {
      x: lastWaypoint.x + curveFactor * (waypoint.x - lastWaypoint.x),
      y: lastWaypoint.y + curveFactor * (waypoint.y - lastWaypoint.y),
    };
    return referencePoint;
  }
}
