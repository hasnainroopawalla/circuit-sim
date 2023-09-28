import config from "../config";
import { State } from "../enums/state";
import { IPosition } from "./render-options.interface";
import Pin from "./pin";

class Wire {
  p: p5;
  startPin: Pin;
  endPin: Pin;
  state: State;
  waypoints: IPosition[];

  constructor(p5: p5, startPin: Pin, endPin: Pin, waypoints: IPosition[]) {
    this.p = p5;
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

export default Wire;
