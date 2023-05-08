import { State } from "../enums/state";
import Chip from "./chip";
import Wire from "./wire";
import InOut from "./inout";
import p5Types from "p5";
import { PinRenderOptions, Position } from "../models/renderOptions";

class Pin {
  p5: p5Types;
  name: string;
  state: State;
  outgoingWires: Wire[];
  chip: Chip | InOut;
  options: PinRenderOptions;

  constructor(p5: p5Types, name: string, state: State, chip: Chip | InOut) {
    this.p5 = p5;
    this.name = name;
    this.state = state;
    this.outgoingWires = [];
    this.chip = chip;
    this.options = {
      position: {
        x: 150,
        y: 160,
      },
      size: 7,
      color: 50,
    };
  }

  propagate() {
    for (let i = 0; i < this.outgoingWires.length; i++) {
      this.outgoingWires[i].propagate();
    }
  }

  render(position: Position) {
    this.p5.fill(this.state === State.Off ? "red" : "green");
    this.p5.circle(position.x, position.y, this.options.size);
  }
}

export default Pin;
