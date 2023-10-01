import { Position, State } from "./shared.interface";

import Chip from "./chip";
import Wire from "./wire";
import IOChip from "./io-chip";
import config from "../config";

type PinRenderOptions = {
  position: Position;
  size: number;
  color: string;
};

class Pin {
  p: p5;
  id: string;
  state: State;
  isInput: boolean; // TODO: change to type: "input" | "output"
  outgoingWires: Wire[];
  chip: Chip | IOChip;
  options: PinRenderOptions;

  constructor(
    p5: p5,
    id: string,
    state: State,
    isInput: boolean,
    chip: Chip | IOChip
  ) {
    this.p = p5;
    this.id = id;
    this.state = state;
    this.isInput = isInput;
    this.outgoingWires = [];
    this.chip = chip;
    this.options = {
      position: { x: 0, y: 0 },
      size: config.component.pin.size,
      color: config.component.pin.color,
    };
  }

  public isMouseOver(): boolean {
    return (
      this.p.dist(
        this.p.mouseX,
        this.p.mouseY,
        this.options.position.x,
        this.options.position.y
      ) <=
      this.options.size + config.component.pin.mouse.hitRange
    );
  }

  public propagate(): void {
    for (let i = 0; i < this.outgoingWires.length; i++) {
      this.outgoingWires[i].propagate();
    }
  }

  public setPosition(position: Position): void {
    this.options.position = position;
  }

  public mouseClicked(): boolean {
    return this.isMouseOver();
  }

  public render(): void {
    this.p.push();
    this.p.stroke(config.component.circuit.background);
    this.p.strokeWeight(config.component.pin.strokeWeight);
    this.p.fill(config.component.pin.color);
    this.p.circle(
      this.options.position.x,
      this.options.position.y,
      this.options.size
    );
    this.p.pop();
  }
}

export default Pin;
