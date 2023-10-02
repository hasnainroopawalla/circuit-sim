import { Position, Size, State } from "./shared.interface";

import config from "../config";
import { textPositionInRect, inputPinsPosition, chipSize } from "./utils";
import Circuit from "./circuit";
import Pin from "./pin";

type ChipRenderOptions = {
  position: Position;
  size: Size;
  color: string;
  textColor: string;
  textPosition: Position;
};

class Chip {
  p: p5;
  inputPins: Pin[] = [];
  outputPins: Pin[] = [];
  name: string;
  id: string;
  action: (a: Pin[]) => State[];
  options: ChipRenderOptions;
  isCircuit: boolean;
  circuit?: Circuit;

  constructor(
    p5: p5,
    name: string,
    id: string,
    numInputPins: number,
    numOutputPins: number,
    action: (a: Pin[]) => State[],
    color: string,
    isCircuit: boolean,
    circuit?: Circuit
  ) {
    this.p = p5;
    this.name = name;
    this.id = id;
    this.action = action;
    this.isCircuit = isCircuit;
    this.circuit = circuit;
    if (isCircuit && this.circuit) {
      // console.log("CHIP", this.circuit);
      this.inputPins = this.circuit.inputs.map((input) => input.pin);
      this.outputPins = this.circuit.outputs.map((output) => output.pin);
      for (let i = 0; i < this.inputPins.length; i++) {
        this.inputPins[i].id = `${id}_input-pin-${i}`;
        this.inputPins[i].isInput = true;
        // this.inputPins[i].chip = this;
      }
      for (let i = 0; i < this.outputPins.length; i++) {
        this.outputPins[i].id = `${id}_output-pin-${i}`;
        this.outputPins[i].isInput = false;
        // this.outputPins[i].chip = this;
      }
    } else {
      for (let i = 0; i < numInputPins; i++) {
        this.inputPins.push(
          new Pin(p5, `${id}_input-pin-${i}`, State.Off, true, this)
        );
      }
      for (let i = 0; i < numOutputPins; i++) {
        this.outputPins.push(
          new Pin(p5, `${id}_output-pin-${i}`, State.Off, false, this)
        );
      }
    }

    const size = chipSize(
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
      textPosition: { x: 0, y: 0 },
      color: color,
      textColor: config.component.chip.text.color,
    };

    // console.log("FINAL", this.name, this);
  }

  private renderPins(): void {
    const inputPinsPositions = inputPinsPosition(
      this.options.position,
      {
        x: this.options.position.x,
        y: this.options.position.y + this.options.size.h,
      },
      this.inputPins.length
    );
    const outputPinsPositions = inputPinsPosition(
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

  private renderChip(): void {
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

  private isMouseOver(): boolean {
    return (
      this.p.mouseX >= this.options.position.x &&
      this.p.mouseX <= this.options.position.x + this.options.size.w &&
      this.p.mouseY >= this.options.position.y &&
      this.p.mouseY <= this.options.position.y + this.options.size.h
    );
  }

  public getPinById(pinId: string): Pin | undefined {
    return [...this.inputPins, ...this.outputPins].find(
      ({ id }) => id === pinId
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

  public execute(): void {
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

  public mouseDragged(): void {
    this.options.position = {
      x: this.p.mouseX - this.options.size.w / 2,
      y: this.p.mouseY - this.options.size.h / 2,
    };
  }

  public render(): void {
    // if (this.circuit) {
    //   console.log(this.inputPins, this.outputPins);
    // }
    this.renderChip();
    this.renderText();
    this.renderPins();
    this.p.push();
    this.p.fill(255);
    this.p.text(this.id, this.options.position.x, this.options.position.y);
    this.p.pop();
  }
}

export default Chip;
