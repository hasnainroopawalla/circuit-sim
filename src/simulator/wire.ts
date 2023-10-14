import { State, Position } from "./shared.interface";

import { config } from "../config";
import { Pin } from "./pin";

export type WireMarker = { referencePoint: Position; waypoint: Position };

export class Wire {
  p: p5;
  startPin: Pin;
  endPin: Pin;
  state: State;
  markers: WireMarker[];

  constructor(p5: p5, startPin: Pin, endPin: Pin, markers: WireMarker[] = []) {
    this.p = p5;
    this.startPin = startPin;
    this.endPin = endPin;
    this.state = State.Off;
    this.markers = markers;
  }

  public propagate(): void {
    this.state = this.startPin.state;
    this.endPin.state = this.startPin.state;
    this.endPin.chip.execute();
  }

  public render(): void {
    this.p.push();
    this.p.strokeWeight(config.component.wire.strokeWeight);
    this.p.stroke(
      this.state === State.Off
        ? config.component.wire.color.stateOff
        : config.component.wire.color.stateOn
    );
    this.p.noFill();

    // TODO: fix https://github.com/hasnainroopawalla/circuit-sim/issues/15
    const tempMarkers = [
      {
        referencePoint: this.startPin.options.position,
        waypoint: this.startPin.options.position,
      },
      ...this.markers,
    ];

    for (let i = 0; i < tempMarkers.length; i++) {
      const startPoint = tempMarkers[i].referencePoint;

      const controlPoint = {
        x: tempMarkers[i].waypoint.x,
        y: tempMarkers[i].waypoint.y,
      };

      const endPoint =
        i === tempMarkers.length - 1
          ? this.endPin.options.position
          : tempMarkers[i + 1].referencePoint;

      this.p.bezier(
        startPoint.x,
        startPoint.y,
        controlPoint.x,
        controlPoint.y,
        controlPoint.x,
        controlPoint.y,
        endPoint.x,
        endPoint.y
      );
    }
    this.p.pop();
  }
}
