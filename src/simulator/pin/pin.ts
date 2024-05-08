import { State } from "../common";

import { Chip, IOChip } from "../chips";
import { Wire } from "../wire";
import { Position } from "../common";
import { config } from "../config";
import { pinConfig } from "./pin.config";

export class Pin {
  p: p5;
  name: string;
  id: number;
  state: State;
  isInput: boolean; // TODO: change to type: "input" | "output"
  isGhost: boolean;
  outgoingWires: Wire[];
  chip: Chip | IOChip;
  position: Position;
  size: number;

  constructor(
    p5: p5,
    id: number,
    state: State,
    isInput: boolean,
    chip: Chip | IOChip,
    isGhost = false
  ) {
    this.p = p5;
    this.id = id;
    this.state = state;
    this.isInput = isInput;
    this.outgoingWires = [];
    this.chip = chip;
    this.name =
      this.chip instanceof IOChip ? "pin" : `${isInput ? "In" : "Out"} ${id}`;
    this.isGhost = isGhost;
    this.position = { x: 0, y: 0 };
    this.size = pinConfig.size;
  }

  public isMouseOver(): boolean {
    return (
      this.p.dist(
        this.p.mouseX,
        this.p.mouseY,
        this.position.x,
        this.position.y
      ) <=
      this.size / 2
    );
  }

  public propagate(): void {
    for (const outgoingWire of this.outgoingWires) {
      outgoingWire.propagate();
    }
  }

  public setPosition(position: Position): void {
    this.position = position;
  }

  public mouseClicked(): boolean {
    return this.isMouseOver();
  }

  public render(): void {
    this.p.push();
    this.p.stroke(pinConfig.strokeColor);
    this.p.strokeWeight(pinConfig.strokeWeight);

    if (this.isMouseOver()) {
      this.p.textSize(12);
      const textWidth = this.p.textWidth(this.name) + 5;

      const rect = {
        x: this.isInput
          ? this.position.x - this.size - textWidth
          : this.position.x + this.size / 2 + 4,
        y: this.position.y - this.size + 6,
        w: textWidth + 5,
        h: 18,
      };
      this.p.noStroke();
      this.p.fill(pinConfig.color);
      this.p.rect(rect.x, rect.y, rect.w, rect.h);
      this.p.fill("white");
      this.p.textAlign(this.p.CENTER);
      this.p.text(this.name, rect.x + rect.w / 2, rect.y + rect.h / 2 + 4);
    } else {
      this.isGhost
        ? this.p.fill(config.ghostEntityColor)
        : this.p.fill(pinConfig.color);
    }
    this.p.circle(this.position.x, this.position.y, this.size);
    this.p.pop();
  }
}
