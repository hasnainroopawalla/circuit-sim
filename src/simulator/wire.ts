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
    this.p.line(
      this.startPin.options.position.x,
      this.startPin.options.position.y,
      this.endPin.options.position.x,
      this.endPin.options.position.y
    );
    this.p.pop();
  }
}
