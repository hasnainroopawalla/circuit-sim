import { State } from "../enums/state";
import Pin from "./pin";
import Wire from "./wire";
import p5Types from "p5";

class InOut {
  p5: p5Types;
  name: string;
  isInput: boolean;
  pin: Pin;
  outgoingWires: Wire[];

  constructor(p5: p5Types, name: string, isInput: boolean) {
    this.p5 = p5;
    this.name = name;
    this.isInput = isInput;
    this.pin = new Pin(p5, name, State.Off, this);
    this.outgoingWires = [];
  }

  toggle() {
    this.pin.state = this.pin.state === State.Off ? State.On : State.Off;
  }

  execute() {
    this.pin.propagate();
  }
}

export default InOut;
