import { Position, Size } from "../shared.interface";

// import { config } from "../../config";
import { ChipHelper } from "../helpers/chip-helper";
import { Pin } from "../pin";

const config = {
  strokeWeight: 0,
  size: {
    cornerRadius: 5,
  },
  text: {
    size: 20,
    color: "#FFFFFF",
  },
};

type ChipRenderOptions = {
  position: Position;
  size: Size;
  color: string;
  textColor: string;
  textPosition: Position;
};

export abstract class Chip {
  p: p5;
  inputPins: Pin[] = [];
  outputPins: Pin[] = [];
  name: string;
  id: string;
  options: ChipRenderOptions;

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

    const size = ChipHelper.chipSize(
      this.name,
      config.text.size,
      Math.max(numInputPins, numOutputPins)
    );

    this.options = {
      position: {
        x: this.p.mouseX - size.w / 2,
        y: this.p.mouseY - size.h / 2,
      },
      size,
      textPosition: { x: 0, y: 0 },
      color: color,
      textColor: config.text.color,
    };
  }

  private renderPins(): void {
    const inputPinsPositions = ChipHelper.pinsPositions(
      this.options.position,
      {
        x: this.options.position.x,
        y: this.options.position.y + this.options.size.h,
      },
      this.inputPins.length
    );
    const outputPinsPositions = ChipHelper.pinsPositions(
      {
        x: this.options.position.x + this.options.size.w,
        y: this.options.position.y,
      },
      {
        x: this.options.position.x + this.options.size.w,
        y: this.options.position.y + this.options.size.h,
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

  private renderText(): void {
    this.p.push();
    this.p.textStyle(this.p.BOLD);
    this.options.textPosition = ChipHelper.textPositionInRect(
      this.options.position,
      this.options.size
    );
    this.p.fill(this.options.textColor);
    this.p.textAlign(this.p.CENTER, this.p.CENTER);
    this.p.textSize(config.text.size);
    this.p.text(
      this.name,
      this.options.textPosition.x,
      this.options.textPosition.y
    );
    this.p.pop();
  }

  private renderChip(): void {
    this.p.push();
    this.p.fill(this.options.color);
    this.p.strokeWeight(config.strokeWeight);
    this.p.rect(
      this.options.position.x,
      this.options.position.y,
      this.options.size.w,
      this.options.size.h,
      config.size.cornerRadius
    );
    this.p.pop();
  }

  private isMouseOver(): boolean {
    return (
      this.p.mouseX >= this.options.position.x &&
      this.p.mouseX <= this.options.position.x + this.options.size.w &&
      this.p.mouseY >= this.options.position.y &&
      this.p.mouseY <= this.options.position.y + this.options.size.h
    );
  }

  public getPin(type: string, pinId: number): Pin | undefined {
    // TODO: convert type to "input" | "output"
    return type === "input" ? this.inputPins[pinId] : this.outputPins[pinId];
  }

  public isMouseOverGetEntity(): Chip | Pin | undefined {
    for (let i = 0; i < this.inputPins.length; i++) {
      const pin = this.inputPins[i];
      if (pin.isMouseOver()) {
        return pin;
      }
    }
    for (let i = 0; i < this.outputPins.length; i++) {
      const pin = this.outputPins[i];
      if (pin.isMouseOver()) {
        return pin;
      }
    }
    if (this.isMouseOver()) {
      return this;
    }
  }

  public mouseDragged(): void {
    this.options.position = {
      x: this.p.mouseX - this.options.size.w / 2,
      y: this.p.mouseY - this.options.size.h / 2,
    };
  }

  public render(): void {
    this.renderChip();
    this.renderText();
    this.renderPins();
  }

  abstract execute(): void;
}
