import p5 from "p5";
import { IOChip } from "./io-chip";
import { sliderPosition } from "./io-chip-renderer-utils";
import { iOChipConfig, sliderConfig } from "./io-chip.config";
import { Position, Size } from "../../types";
import { config } from "../../entity.config";

type IIOSliderArgs = {
  p: p5;
  chip: IOChip;
};

export class IOSlider {
  p: p5;
  chip: IOChip;
  position: Position;
  size: Size<"rect">;

  constructor(args: IIOSliderArgs) {
    const { p, chip } = args;

    this.p = p;
    this.chip = chip;
    const { position, size } = sliderPosition({
      sliderPadding: sliderConfig.padding,
      chipPosition: chip.renderer.position,
      chipSize: iOChipConfig.size,
      windowWidth: this.p.windowWidth,
      isInput: chip.isInput,
    });
    this.position = position;
    this.size = size;
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
      ? this.p.fill(config.ghostEntityColor)
      : this.isMouseOver()
      ? this.p.fill("white")
      : this.p.fill(sliderConfig.color);
    this.p.rect(this.position.x, this.position.y, this.size.w, this.size.h);
    this.p.pop();
  }
}
