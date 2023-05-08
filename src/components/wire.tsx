import { State } from "../enums/state";
import Pin from "./pin";

class Wire {
  startPin: Pin;
  endPin: Pin;
  state: State;

  constructor(startPin: Pin, endPin: Pin) {
    this.startPin = startPin;
    this.endPin = endPin;
    this.state = State.Off;
  }

  propagate() {
    this.endPin.state = this.startPin.state;
    this.endPin.chip.execute();
  }
}

export default Wire;
