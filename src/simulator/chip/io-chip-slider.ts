import { IOChipHelper } from "../helpers/io-chip-helper";
import type { Position, Size } from "../shared.interface";
import { config as sharedConfig } from "../../config";
import { IOChip, iOChipConfig } from "./io-chip";

export const sliderConfig = {
  color: "#4A4A4A",
  padding: 4,
};

export class IOSlider {
  p: p5;
  chip: IOChip;
  position: Position;
  size: Size;

  constructor(p: p5, chip: IOChip) {
    this.p = p;
    this.chip = chip;
    const { position, size } = IOChipHelper.sliderPosition(
      sliderConfig.padding,
      chip.position,
      iOChipConfig.size,
      this.p.windowWidth,
      chip.isInput
    );
    [this.position, this.size] = [position, size];
  }

  public isMouseOver(): boolean {
    return (
      this.p.mouseX >= this.position.x &&
      this.p.mouseX <= this.position.x + this.size.w &&
      this.p.mouseY >= this.position.y &&
      this.p.mouseY <= this.position.y + this.size.h
    );
  }

  public setPosition(position: Position): void {
    this.position = position;
  }

  public render(): void {
    this.p.push();
    this.p.strokeWeight(0);
    this.chip.isGhost
      ? this.p.fill(sharedConfig.ghostEntityColor)
      : this.isMouseOver()
      ? this.p.fill("white")
      : this.p.fill(sliderConfig.color);
    this.p.rect(this.position.x, this.position.y, this.size.w, this.size.h);
    this.p.pop();
  }
}
