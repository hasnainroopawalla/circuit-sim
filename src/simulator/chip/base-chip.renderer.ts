import {
  type Position,
  AbstractRenderer,
} from "../api/abstract-renderer";
import { ChipHelper } from "../helpers/chip-helper";
import { baseChipConfig } from "./base-chip.config";

export class BaseChipRenderer extends AbstractRenderer {
  color: string;
  name: string;
  textColor: string;
  textPosition: Position;

  numInputPins: number;
  numOutputPins: number;

  constructor(
    p: p5,
    name: string,
    color: string,
    numInputPins: number,
    numOutputPins: number
  ) {
    const size = ChipHelper.chipSize(
      name,
      baseChipConfig.text.size,
      Math.max(numInputPins, numOutputPins)
    );

    const position = {
      x: p.mouseX - size.w / 2,
      y: p.mouseY - size.h / 2,
    };

    super(p, position, size);

    this.color = color;
    this.name = name;
    this.textColor = baseChipConfig.text.color;
    this.textPosition = { x: 0, y: 0 };

    this.numInputPins = numInputPins;
    this.numOutputPins = numOutputPins;
  }

  public isMouseOver() {
    return (
      this.p.mouseX >= this.position.x &&
      this.p.mouseX <= this.position.x + this.size.w &&
      this.p.mouseY >= this.position.y &&
      this.p.mouseY <= this.position.y + this.size.h
    );
  }

  public mouseDragged() {
    this.position = {
      x: this.p.mouseX - this.size.w / 2,
      y: this.p.mouseY - this.size.h / 2,
    };
  }

  public setPosition(position: Position) {
    this.position = position;
  }

  public render() {
    this.renderChip();
    this.renderText();
  }

  private renderChip(): void {
    this.p.push();
    this.p.fill(this.color);
    this.p.strokeWeight(baseChipConfig.strokeWeight);
    this.p.rect(
      this.position.x,
      this.position.y,
      this.size.w,
      this.size.h,
      baseChipConfig.size.cornerRadius
    );
    this.p.pop();
  }

  private renderText(): void {
    this.p.push();
    this.p.textStyle(this.p.BOLD);
    this.textPosition = ChipHelper.textPositionInRect(this.position, this.size);
    this.p.fill(this.textColor);
    this.p.textAlign(this.p.CENTER, this.p.CENTER);
    this.p.textSize(baseChipConfig.text.size);
    this.p.text(this.name, this.textPosition.x, this.textPosition.y);
    this.p.pop();
  }
}
