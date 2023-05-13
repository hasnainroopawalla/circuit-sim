import { State } from "../enums/state";
import { BasicGate } from "../models/basicGates";
import { ICircuitState } from "../models/circuitState";
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
  state: ICircuitState;
  basicGates: { [chip: string]: BasicGate };

  constructor(p5: p5Types) {
    this.p5 = p5;
    this.inputs = [];
    this.outputs = [];
    this.wires = [];
    this.chips = [];
    this.state = { wiringMode: { enabled: false } };
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

  private toggleWiringMode(pin: Pin) {
    if (this.state.wiringMode.enabled && this.state.wiringMode.startPin) {
      this.addWire(this.state.wiringMode.startPin, pin);
      this.state.wiringMode = {
        enabled: false,
        startPin: undefined,
        endPin: undefined,
      };
    } else {
      this.state.wiringMode = {
        enabled: true,
        startPin: pin,
      };
    }
  }

  private renderWiringModeWire() {
    this.p5.strokeWeight(3);
    this.p5.line(
      this.state.wiringMode.startPin!.options.position.x,
      this.state.wiringMode.startPin!.options.position.y,
      this.p5.mouseX,
      this.p5.mouseY
    );
    this.p5.strokeWeight(1);
  }

  private renderIOChips() {
    for (let i = 0; i < this.inputs.length; i++) {
      this.inputs[i].render();
    }
    for (let i = 0; i < this.outputs.length; i++) {
      this.outputs[i].render();
    }
  }

  private renderChips() {
    for (let i = 0; i < this.chips.length; i++) {
      this.chips[i].render();
    }
  }

  private renderWires() {
    for (let i = 0; i < this.wires.length; i++) {
      this.p5.strokeWeight(3);
      this.p5.line(
        this.wires[i].startPin.options.position.x,
        this.wires[i].startPin.options.position.y,
        this.wires[i].endPin.options.position.x,
        this.wires[i].endPin.options.position.y
      );
    }
    this.p5.strokeWeight(1);
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
    // Input IOChips
    for (let i = 0; i < this.inputs.length; i++) {
      const entity = this.inputs[i].mouseClicked();
      if (entity instanceof Pin) {
        this.toggleWiringMode(entity);
      }
    }
    // Output IOChips
    for (let i = 0; i < this.outputs.length; i++) {
      const entity = this.outputs[i].mouseClicked();
      if (entity instanceof Pin) {
        this.toggleWiringMode(entity);
      }
    }
    // Chips
    for (let i = 0; i < this.chips.length; i++) {
      const entity = this.chips[i].mouseClicked();
      if (entity instanceof Pin) {
        this.toggleWiringMode(entity);
      }
    }
  }

  mousePressed() {
    console.log("press");
  }

  mouseReleased() {
    console.log("release");
  }

  render() {
    this.renderChips();
    this.renderIOChips();
    this.renderWires();
    if (this.state.wiringMode.enabled) {
      this.renderWiringModeWire();
    }
  }
}

export default Circuit;
