import { State } from "../enums/state";
import { ChipRenderOptions, Position, Size } from "../models/renderOptions";
import {
  computeChipTextPosition,
  computeInputPinsPosition,
} from "../utils/position";
import Pin from "./pin";
import p5Types from "p5";

class Chip {
  p5: p5Types;
  inputPins: Pin[] = [];
  outputPins: Pin[] = [];
  name: string;
  action: (a: Pin[]) => State[];
  options: ChipRenderOptions;

  constructor(
    p5: p5Types,
    name: string,
    numInputPins: number,
    numOutputPins: number,
    action: (a: Pin[]) => State[]
  ) {
    this.p5 = p5;
    this.name = name;
    this.action = action;
    for (let i = 0; i < numInputPins; i++) {
      this.inputPins.push(
        new Pin(p5, `${name}_INPUT_${this.inputPins.length}`, State.Off, this)
      );
    }
    for (let i = 0; i < numOutputPins; i++) {
      this.outputPins.push(
        new Pin(p5, `${name}_OUTPUT_${this.outputPins.length}`, State.Off, this)
      );
    }

    const position: Position = {
      x: Math.random() * 250,
      y: Math.random() * 250,
    };
    const size: Size = {
      w: this.p5.textWidth(this.name) + 30,
      h: Math.max(numInputPins, numOutputPins) * 30,
    };

    this.options = {
      position,
      size,
      textPosition: computeChipTextPosition(position, size),
      textSize: 12,
      color: 10,
      textColor: 200,
    };
  }

  private renderPins() {
    const inputPinsPositions = computeInputPinsPosition(
      this.options.position,
      {
        x: this.options.position.x,
        y: this.options.position.y + this.options.size.h,
      },
      this.inputPins.length
    );
    const outputPinsPositions = computeInputPinsPosition(
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

  private renderText() {
    this.options.textPosition = computeChipTextPosition(
      this.options.position,
      this.options.size
    );
    this.p5.fill(this.options.textColor);
    this.p5.textAlign(this.p5.CENTER, this.p5.CENTER);
    this.p5.textSize(this.options.textSize);
    this.p5.text(
      this.name,
      this.options.textPosition.x,
      this.options.textPosition.y
    );
  }

  private renderChip() {
    this.p5.fill(this.options.color);
    this.p5.rect(
      this.options.position.x,
      this.options.position.y,
      this.options.size.w,
      this.options.size.h,
      5,
      5,
      5,
      5
    );
  }

  getInputPin = (idx: number) => this.inputPins[idx];

  getOutputPin = (idx: number) => this.outputPins[idx];

  execute() {
    const outputStates = this.action(this.inputPins);
    for (let i = 0; i < this.outputPins.length; i++) {
      this.outputPins[i].state = outputStates[i];
      this.outputPins[i].propagate();
    }
  }

  mouseClicked() {
    // Input pins
    for (let i = 0; i < this.inputPins.length; i++) {
      if (this.inputPins[i].mouseClicked()) {
        return this.inputPins[i];
      }
    }
    // Output pins
    for (let i = 0; i < this.outputPins.length; i++) {
      if (this.outputPins[i].mouseClicked()) {
        return this.outputPins[i];
      }
    }
  }

  render() {
    this.renderChip();
    this.renderText();
    this.renderPins();
  }
}

export default Chip;
