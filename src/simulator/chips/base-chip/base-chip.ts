import { Position } from "../../common";
import { Pin } from "../../pin";
import { pinsPositions } from "./base-chip-renderer-utils";
import { BaseChipRenderer } from "./base-chip.renderer";

export abstract class BaseChip {
  p: p5;
  inputPins: Pin[] = [];
  outputPins: Pin[] = [];
  name: string;
  id: string;

  renderer: BaseChipRenderer;

  constructor(
    p: p5,
    name: string,
    id: string,
    numInputPins: number,
    numOutputPins: number,
    color: string
  ) {
    this.p = p;
    this.name = name;
    this.id = id;

    this.renderer = new BaseChipRenderer(
      p,
      name,
      color,
      numInputPins,
      numOutputPins
    );
  }

  public getPin(type: string, pinId: number): Pin | undefined {
    // TODO: convert type to "input" | "output"
    return type === "input" ? this.inputPins[pinId] : this.outputPins[pinId];
  }

  public isMouseOverGetEntity(): BaseChip | Pin | undefined {
    for (const pin of this.inputPins) {
      if (pin.isMouseOver()) {
        return pin;
      }
    }

    for (const pin of this.outputPins) {
      if (pin.isMouseOver()) {
        return pin;
      }
    }

    if (this.renderer.isMouseOver()) {
      return this;
    }
  }

  public mouseDragged(): void {
    this.renderer.mouseDragged();
  }

  public setPosition(position: Position) {
    this.renderer.setPosition(position);
  }

  public render(): void {
    this.renderer.render();
    this.renderPins();
  }

  // TODO: move to renderer
  private renderPins(): void {
    const inputPinsPositions = pinsPositions(
      this.renderer.position,
      {
        x: this.renderer.position.x,
        y: this.renderer.position.y + this.renderer.size.h,
      },
      this.inputPins.length
    );

    const outputPinsPositions = pinsPositions(
      {
        x: this.renderer.position.x + this.renderer.size.w,
        y: this.renderer.position.y,
      },
      {
        x: this.renderer.position.x + this.renderer.size.w,
        y: this.renderer.position.y + this.renderer.size.h,
      },
      this.outputPins.length
    );

    for (let i = 0; i < this.inputPins.length; i++) {
      this.inputPins[i].setPosition(inputPinsPositions[i]);
      this.inputPins[i].render();
    }
    for (let i = 0; i < this.outputPins.length; i++) {
      this.outputPins[i].setPosition(outputPinsPositions[i]);
      this.outputPins[i].render();
    }
  }

  public abstract execute(): void;
}
