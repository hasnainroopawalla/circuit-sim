import { Position, State } from "./shared.interface";

import { Chip, IOChip } from "./chip";
import { Wire } from "./wire";
import { config as sharedConfig } from "../config";

const config = {
  color: "#121212",
  size: 15,
  strokeColor: "#3D3D3D",
  strokeWeight: 2,
};

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
    this.size = config.size;
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
    for (let i = 0; i < this.outgoingWires.length; i++) {
      this.outgoingWires[i].propagate();
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
    this.p.stroke(config.strokeColor);
    this.p.strokeWeight(config.strokeWeight);

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
      this.p.fill(config.color);
      this.p.rect(rect.x, rect.y, rect.w, rect.h);
      this.p.fill("white");
      this.p.textAlign(this.p.CENTER);
      this.p.text(this.name, rect.x + rect.w / 2, rect.y + rect.h / 2 + 4);
    } else {
      this.isGhost
        ? this.p.fill(sharedConfig.ghostEntityColor)
        : this.p.fill(config.color);
    }
    this.p.circle(this.position.x, this.position.y, this.size);
    this.p.pop();
  }
}
