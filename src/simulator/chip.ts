import type { IChipRenderOptions, ISize } from "./render-options.interface";

import config from "../config";
import { State } from "../enums/state";
import {
  textPositionInRect,
  computeInputPinsPosition,
  computeChipSize,
} from "../utils/Position";
import { initPosition } from "../utils/Utils";
import Circuit from "./circuit";
import Pin from "./pin";

class Chip {
  p: p5;
  inputPins: Pin[] = [];
  outputPins: Pin[] = [];
  name: string;
  action: (a: Pin[]) => State[];
  options: IChipRenderOptions;
  isCircuit: boolean;
  circuit?: Circuit;

  constructor(
    p5: p5,
    name: string,
    numInputPins: number,
    numOutputPins: number,
    action: (a: Pin[]) => State[],
    color: string,
    isCircuit: boolean,
    circuit?: Circuit
  ) {
    this.p = p5;
    this.name = name;
    this.action = action;
    this.isCircuit = isCircuit;
    this.circuit = circuit;
    if (isCircuit && this.circuit) {
      this.inputPins = this.circuit.inputs.map((input) => input.pin);
      this.outputPins = this.circuit.outputs.map((output) => output.pin);
      for (let i = 0; i < this.inputPins.length; i++) {
        this.inputPins[i].name = `${name}_INPUT_${i}`;
        this.inputPins[i].isInput = true;
      }
      for (let i = 0; i < this.outputPins.length; i++) {
        this.outputPins[i].name = `${name}_OUTPUT_${i}`;
        this.outputPins[i].isInput = false;
      }
    } else {
      for (let i = 0; i < numInputPins; i++) {
        this.inputPins.push(
          new Pin(p5, `${name}_INPUT_${i}`, State.Off, true, this)
        );
      }
      for (let i = 0; i < numOutputPins; i++) {
        this.outputPins.push(
          new Pin(p5, `${name}_OUTPUT_${i}`, State.Off, false, this)
        );
      }
    }

    const size: ISize = computeChipSize(
      this.name,
      config.component.chip.text.size,
      Math.max(this.inputPins.length, this.outputPins.length)
    );

    this.options = {
      position: {
        x: this.p.mouseX - size.w / 2,
        y: this.p.mouseY - size.h / 2,
      },
      size,
      textPosition: initPosition(),
      color: color,
      textColor: config.component.chip.text.color,
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
    this.p.push();
    this.p.textStyle(this.p.BOLD);
    this.options.textPosition = textPositionInRect(
      this.options.position,
      this.options.size
    );
    this.p.fill(this.options.textColor);
    this.p.textAlign(this.p.CENTER, this.p.CENTER);
    this.p.textSize(config.component.chip.text.size);
    this.p.text(
      this.name,
      this.options.textPosition.x,
      this.options.textPosition.y
    );
    this.p.pop();
  }

  private renderChip() {
    this.p.push();
    this.p.fill(this.options.color);
    this.p.strokeWeight(config.component.chip.strokeWeight);
    this.p.rect(
      this.options.position.x,
      this.options.position.y,
      this.options.size.w,
      this.options.size.h,
      config.component.chip.size.cornerRadius
    );
    this.p.pop();
  }

  isMouseOver() {
    return (
      this.p.mouseX >= this.options.position.x &&
      this.p.mouseX <= this.options.position.x + this.options.size.w &&
      this.p.mouseY >= this.options.position.y &&
      this.p.mouseY <= this.options.position.y + this.options.size.h
    );
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

  execute() {
    if (this.isCircuit && this.circuit) {
      this.circuit.execute();
    } else {
      const outputStates = this.action(this.inputPins);
      for (let i = 0; i < this.outputPins.length; i++) {
        this.outputPins[i].state = outputStates[i];
        this.outputPins[i].propagate();
      }
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

  mouseDragged() {
    this.options.position = {
      x: this.p.mouseX - this.options.size.w / 2,
      y: this.p.mouseY - this.options.size.h / 2,
    };
  }

  render() {
    this.renderChip();
    this.renderText();
    this.renderPins();
  }
}

export default Chip;
