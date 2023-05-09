import { State } from "../enums/state";
import { BasicGate } from "../models/basicGates";
import Chip from "./chip";
import IOChip from "./io";
import Pin from "./pin";
import Wire from "./wire";
import p5Types from "p5";

class Circuit {
  p5: p5Types;
  inputs: IOChip[];
  outputs: IOChip[];
  wires: Wire[];
  chips: Chip[];
  wiringMode: boolean;
  basicGates: { [chip: string]: BasicGate };

  constructor(p5: p5Types) {
    this.p5 = p5;
    this.inputs = [];
    this.outputs = [];
    this.wires = [];
    this.chips = [];
    this.wiringMode = false;
    this.basicGates = {
      AND: {
        inputPins: 2,
        outputPins: 1,
        action: (inputPins: Pin[]) => [
          inputPins[0].state && inputPins[1].state,
        ],
      },
      OR: {
        inputPins: 2,
        outputPins: 1,
        action: (inputPins: Pin[]) => [
          inputPins[0].state || inputPins[1].state,
        ],
      },
      NOT: {
        inputPins: 1,
        outputPins: 1,
        action: (inputPins: Pin[]) => [
          inputPins[0].state === State.On ? State.Off : State.On,
        ],
      },
    };
  }

  getInputPin = (idx: number): IOChip => this.inputs[idx];

  getOutputPin = (idx: number): IOChip => this.outputs[idx];

  getChip = (idx: number): Chip => this.chips[idx];

  addInputPin(name: string) {
    this.inputs.push(new IOChip(this.p5, name, true));
  }

  addOutputPin(name: string) {
    this.outputs.push(new IOChip(this.p5, name, false));
  }

  addChip(chipName: string) {
    const basicGate = this.basicGates[chipName];
    this.chips.push(
      new Chip(
        this.p5,
        chipName,
        basicGate.inputPins,
        basicGate.outputPins,
        basicGate.action
      )
    );
    // this.chipPosition.x = this.chipPosition.x + 70;
  }

  addWire(startPin: Pin, endPin: Pin) {
    const wire = new Wire(startPin, endPin);
    this.wires.push(wire);
    startPin.outgoingWires.push(wire);
  }

  execute() {
    for (let i = 0; i < this.inputs.length; i++) {
      this.inputs[i].execute();
    }
  }

  mouseClicked() {
    for (let i = 0; i < this.inputs.length; i++) {
      this.inputs[i].mouseClicked();
    }
    for (let i = 0; i < this.chips.length; i++) {
      this.chips[i].mouseClicked();
    }
  }

  mousePressed() {
    // for (let i = 0; i < this.chips.length; i++) {
    //   this.chips[i].render();
    // }
    console.log("press");
  }

  mouseReleased() {
    // for (let i = 0; i < this.chips.length; i++) {
    //   this.chips[i].render();
    // }
    console.log("release");
  }

  render() {
    for (let i = 0; i < this.inputs.length; i++) {
      this.inputs[i].render();
    }
    for (let i = 0; i < this.outputs.length; i++) {
      this.outputs[i].render();
    }
    for (let i = 0; i < this.chips.length; i++) {
      this.chips[i].render();
    }
  }
}

export default Circuit;
