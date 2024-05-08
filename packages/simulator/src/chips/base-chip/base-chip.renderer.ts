import p5 from "p5";
import { type Position, AbstractRenderer, type Size } from "../../common";
import type { Pin } from "../../pin";
import type { BaseChip } from "./base-chip";
import {
  chipSize,
  pinsPositions,
  textPositionInRect,
} from "./base-chip-renderer-utils";
import { baseChipConfig } from "./base-chip.config";

export class BaseChipRenderer extends AbstractRenderer<Size<"rect">> {
  baseChip: BaseChip;
  color: string;
  textPosition: Position;

  constructor(p: p5, baseChip: BaseChip, color: string) {
    const size = chipSize(
      baseChip.name,
      baseChipConfig.text.size,
      Math.max(baseChip.numInputPins, baseChip.numOutputPins)
    );

    const position = {
      x: p.mouseX - size.w / 2,
      y: p.mouseY - size.h / 2,
    };

    super(p, position, size);

    this.baseChip = baseChip;
    this.color = color;
    this.textPosition = { x: 0, y: 0 };
  }

  public isMouseOver() {
    return (
      this.p.mouseX >= this.position.x &&
      this.p.mouseX <= this.position.x + this.size.w &&
      this.p.mouseY >= this.position.y &&
      this.p.mouseY <= this.position.y + this.size.h
    );
  }

  public isMouseOverGetEntity(): BaseChip | Pin | undefined {
    for (const pin of this.baseChip.inputPins) {
      if (pin.isMouseOver()) {
        return pin;
      }
    }

    for (const pin of this.baseChip.outputPins) {
      if (pin.isMouseOver()) {
        return pin;
      }
    }

    if (this.isMouseOver()) {
      return this.baseChip;
    }
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
    this.renderPins();
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
    this.textPosition = textPositionInRect(this.position, this.size);
    this.p.fill(baseChipConfig.text.color);
    this.p.textAlign(this.p.CENTER, this.p.CENTER);
    this.p.textSize(baseChipConfig.text.size);
    this.p.text(this.baseChip.name, this.textPosition.x, this.textPosition.y);
    this.p.pop();
  }

  private renderPins(): void {
    const inputPinsPositions = pinsPositions(
      this.position,
      {
        x: this.position.x,
        y: this.position.y + this.size.h,
      },
      this.baseChip.numInputPins
    );

    const outputPinsPositions = pinsPositions(
      {
        x: this.position.x + this.size.w,
        y: this.position.y,
      },
      {
        x: this.position.x + this.size.w,
        y: this.position.y + this.size.h,
      },
      this.baseChip.numOutputPins
    );

    for (let i = 0; i < this.baseChip.numInputPins; i++) {
      this.baseChip.inputPins[i].setPosition(inputPinsPositions[i]);
      this.baseChip.inputPins[i].render();
    }
    for (let i = 0; i < this.baseChip.numOutputPins; i++) {
      this.baseChip.outputPins[i].setPosition(outputPinsPositions[i]);
      this.baseChip.outputPins[i].render();
    }
  }
}
