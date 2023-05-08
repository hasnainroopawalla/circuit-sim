import { State } from "../enums/state";
import Chip from "./chip";
import Wire from "./wire";
import IO from "./io";
import p5Types from "p5";
import { PinRenderOptions, Position } from "../models/renderOptions";

class Pin {
  p5: p5Types;
  name: string;
  state: State;
  outgoingWires: Wire[];
  chip: Chip | IO;
  options: PinRenderOptions;

  constructor(p5: p5Types, name: string, state: State, chip: Chip | IO) {
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
      size: 10,
      color: 50,
    };
  }

  private isMouseOver() {
    return (
      this.p5.dist(
        this.p5.mouseX,
        this.p5.mouseY,
        this.options.position.x,
        this.options.position.y
      ) <=
      this.options.size + 10
    );
  }

  propagate() {
    for (let i = 0; i < this.outgoingWires.length; i++) {
      this.outgoingWires[i].propagate();
    }
  }

  setPosition(position: Position) {
    this.options.position = position;
  }

  render() {
    this.p5.fill(this.state === State.Off ? "red" : "green");
    this.p5.circle(
      this.options.position.x,
      this.options.position.y,
      this.options.size
    );
  }
}

export default Pin;
