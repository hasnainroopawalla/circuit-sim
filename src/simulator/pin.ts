import { State } from "../enums/state";
import Chip from "./chip";
import Wire from "./wire";
import IOChip from "./io-chip";
import { IPinRenderOptions, IPosition } from "./render-options.interface";
import config from "../config";
import { initPosition } from "../utils/Utils";

class Pin {
  p5: p5;
  name: string;
  state: State;
  isInput: boolean;
  outgoingWires: Wire[];
  chip: Chip | IOChip;
  options: IPinRenderOptions;

  constructor(
    p5: p5,
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
      position: initPosition(),
      size: config.component.pin.size,
      color: config.component.pin.color,
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
      this.options.size + config.component.pin.mouse.hitRange
    );
  }

  propagate() {
    for (let i = 0; i < this.outgoingWires.length; i++) {
      this.outgoingWires[i].propagate();
    }
  }

  setPosition(position: IPosition) {
    this.options.position = position;
  }

  mouseClicked() {
    return this.isMouseOver();
  }

  render() {
    this.p5.stroke(config.component.circuit.background);
    this.p5.strokeWeight(config.component.pin.strokeWeight);
    this.p5.fill(config.component.pin.color);
    this.p5.circle(
      this.options.position.x,
      this.options.position.y,
      this.options.size
    );
  }
}

export default Pin;
