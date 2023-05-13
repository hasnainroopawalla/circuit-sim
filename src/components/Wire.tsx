import { State } from "../enums/State";
import Pin from "./Pin";
import p5Types from "p5";

class Wire {
  p5: p5Types;
  startPin: Pin;
  endPin: Pin;
  state: State;

  constructor(p5: p5Types, startPin: Pin, endPin: Pin) {
    this.p5 = p5;
    this.startPin = startPin;
    this.endPin = endPin;
    this.state = State.Off;
  }

  propagate() {
    this.state = this.startPin.state;
    this.endPin.state = this.startPin.state;
    this.endPin.chip.execute();
  }

  render() {
    this.p5.strokeWeight(3);
    this.p5.stroke(this.state === State.Off ? "red" : "green");
    this.p5.line(
      this.startPin.options.position.x,
      this.startPin.options.position.y,
      this.endPin.options.position.x,
      this.endPin.options.position.y
    );
    this.p5.strokeWeight(1);
    this.p5.stroke("black");
  }
}

export default Wire;
