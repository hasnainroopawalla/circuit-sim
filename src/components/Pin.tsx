import { State } from "../enums/State";
import Chip from "./Chip";
import Wire from "./Wire";
import IOChip from "./IOChip";
import p5Types from "p5";
import { PinRenderOptions, Position } from "../models/RenderOptions";

class Pin {
  p5: p5Types;
  name: string;
  state: State;
  isInput: boolean;
  outgoingWires: Wire[];
  chip: Chip | IOChip;
  options: PinRenderOptions;

  constructor(
    p5: p5Types,
    name: string,
    state: State,
    isInput: boolean,
    chip: Chip | IOChip
  ) {
    this.p5 = p5;
    this.name = name;
    this.state = state;
    this.isInput = isInput;
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

  mouseClicked() {
    return this.isMouseOver();
  }

  render() {
    this.p5.fill(this.state === State.Off ? "grey" : "grey");
    this.p5.circle(
      this.options.position.x,
      this.options.position.y,
      this.options.size
    );
    this.p5.strokeWeight(1);
  }
}

export default Pin;
