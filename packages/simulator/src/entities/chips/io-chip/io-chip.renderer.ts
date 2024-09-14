import p5 from "p5";
import { Pin, PinState } from "../../pin";
import { IOSlider } from "./io-chip-slider";
import { iOChipConfig, sliderConfig } from "./io-chip.config";
import { pinPosition, sliderPosition } from "./io-chip-renderer-utils";
import { IOChip } from "./io-chip";
import { AbstractRenderer } from "../../abstract-renderer";
import { Size, Position } from "../../types";
import { config } from "../../entity.config";

export class IOChipRenderer extends AbstractRenderer<Size<"circle">> {
  iOChip: IOChip;

  constructor(p: p5, ioChip: IOChip, position: Position) {
    super(p, position, { d: iOChipConfig.size });
    this.iOChip = ioChip;
  }

  public render(): void {
    this.p.push();
    this.renderSlider();
    this.renderInnerWire();
    this.renderChip();
    this.renderPin();
    this.p.pop();
  }

  public mouseDragged(): void {
    this.position = {
      x: this.position.x,
      y: this.p.mouseY,
    };
  }

  public mouseClicked(): Pin | IOChip | undefined {
    if (this.isMouseOverChip() && this.iOChip.isInput) {
      this.iOChip.toggle();
      return this.iOChip;
    }
    if (this.iOChip.pin.mouseClicked()) {
      return this.iOChip.pin;
    }
  }

  public isMouseOverGetEntity(): IOChip | IOSlider | Pin | undefined {
    return this.isMouseOverChip()
      ? this.iOChip
      : this.iOChip.pin.isMouseOver()
      ? this.iOChip.pin
      : this.iOChip.slider.isMouseOver()
      ? this.iOChip.slider
      : undefined;
  }

  public isMouseOverChip(): boolean {
    return (
      this.p.dist(
        this.p.mouseX,
        this.p.mouseY,
        this.position.x,
        this.position.y
      ) <=
      iOChipConfig.size / 2
    );
  }

  private renderChip(): void {
    if (this.iOChip.isGhost) {
      this.p.strokeWeight(0);
      this.p.fill(config.ghostEntityColor);
    } else {
      this.p.strokeWeight(iOChipConfig.strokeWeight);
      this.p.fill(
        this.iOChip.pin.state === PinState.Low
          ? iOChipConfig.color.stateOff
          : iOChipConfig.color.stateOn
      );
    }
    this.p.circle(this.position.x, this.position.y, iOChipConfig.size);
  }

  private renderInnerWire(): void {
    this.iOChip.isGhost
      ? this.p.stroke(config.ghostEntityColor)
      : this.p.stroke(iOChipConfig.innerWire.color);
    this.p.strokeWeight(iOChipConfig.innerWire.strokeWeight);
    this.p.line(
      this.iOChip.isInput
        ? this.position.x + iOChipConfig.size / 2
        : this.position.x - iOChipConfig.size / 2,
      this.position.y,
      this.iOChip.isInput
        ? this.position.x + iOChipConfig.size
        : this.position.x - iOChipConfig.size,
      this.position.y
    );
  }

  private renderPin(): void {
    this.iOChip.pin.setPosition(
      pinPosition(this.position, iOChipConfig.size, this.iOChip.isInput)
    );
    this.iOChip.pin.render();
  }

  private renderSlider(): void {
    this.iOChip.slider.setPosition(
      sliderPosition(
        sliderConfig.padding,
        this.position,
        iOChipConfig.size,
        this.p.windowWidth,
        this.iOChip.isInput
      ).position
    );
    this.iOChip.slider.render();
  }
}
