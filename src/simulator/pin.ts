import { Position, State } from "./shared.interface";

import { Chip, IOChip } from "./chip";
import { Wire } from "./wire";
import { config } from "../config";

type PinRenderOptions = {
  position: Position;
  size: number;
};

export class Pin {
  p: p5;
  id: number;
  state: State;
  isInput: boolean; // TODO: change to type: "input" | "output"
  outgoingWires: Wire[];
  chip: Chip | IOChip;
  options: PinRenderOptions;

  constructor(
    p5: p5,
    id: number,
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
      this.options.size / 2
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

    if (this.isMouseOver()) {
      this.p.textSize(12);
      const textWidth = this.p.textWidth("In A") + 5;

      const rect = {
        x: this.isInput
          ? this.options.position.x - this.options.size - textWidth
          : this.options.position.x + this.options.size / 2 + 4,
        y: this.options.position.y - this.options.size + 6,
        w: textWidth + 5,
        h: 18,
      };
      this.p.fill(config.component.pin.color);
      this.p.rect(rect.x, rect.y, rect.w, rect.h);
      this.p.noStroke();
      this.p.fill("white");
      this.p.textAlign(this.p.CENTER);
      this.p.text("In A", rect.x + rect.w / 2, rect.y + rect.h / 2 + 4);
    } else {
      this.p.fill(config.component.pin.color);
    }
    this.p.circle(
      this.options.position.x,
      this.options.position.y,
      this.options.size
    );
    this.p.pop();
  }
}
