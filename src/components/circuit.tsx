import { State } from "../enums/state";
import { BasicGate } from "../models/basicGates";
import Chip from "./chip";
import InOutPin from "./inout";
import Pin from "./pin";
import Wire from "./wire";
import p5Types from "p5";

class Circuit {
  p5: p5Types;
  inputPins: InOutPin[];
  outputPins: InOutPin[];
  wires: Wire[];
  chips: Chip[];
  basicGates: { [chip: string]: BasicGate };

  constructor(p5: p5Types) {
    this.p5 = p5;
    this.inputPins = [];
    this.outputPins = [];
    this.wires = [];
    this.chips = [];
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

  getInputPin = (idx: number): InOutPin => this.inputPins[idx];

  getOutputPin = (idx: number): InOutPin => this.outputPins[idx];

  getChip = (idx: number): Chip => this.chips[idx];

  addInputPin(name: string) {
    this.inputPins.push(new InOutPin(this.p5, name, true));
  }

  addOutputPin(name: string) {
    this.outputPins.push(new InOutPin(this.p5, name, false));
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
    for (let i = 0; i < this.inputPins.length; i++) {
      this.inputPins[i].execute();
    }
  }

  render() {
    for (let i = 0; i < this.chips.length; i++) {
      this.chips[i].render();
    }
  }
}

export default Circuit;
