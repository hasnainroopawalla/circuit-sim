import config from "../config";
import { State } from "../enums/State";
import { Position } from "../models/RenderOptions";
import Pin from "./Pin";
import p5Types from "p5";

class Wire {
  p5: p5Types;
  startPin: Pin;
  endPin: Pin;
  state: State;
  waypoints: Position[];

  constructor(p5: p5Types, startPin: Pin, endPin: Pin, waypoints: Position[]) {
    this.p5 = p5;
    this.startPin = startPin;
    this.endPin = endPin;
    this.state = State.Off;
    this.waypoints = waypoints;
  }

  public propagate() {
    this.state = this.startPin.state;
    this.endPin.state = this.startPin.state;
    this.endPin.chip.execute();
  }

  public render() {
    this.p5.strokeWeight(config.component.wire.strokeWeight);
    this.p5.stroke(
      this.state === State.Off
        ? config.component.wire.color.stateOff
        : config.component.wire.color.stateOn
    );
    this.p5.line(
      this.startPin.options.position.x,
      this.startPin.options.position.y,
      this.endPin.options.position.x,
      this.endPin.options.position.y
    );
  }
}

export default Wire;
