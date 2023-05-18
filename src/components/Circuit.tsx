import { State } from "../enums/State";
import { BasicGate } from "../models/BasicGates";
import { CircuitState } from "../models/CircuitState";
import Chip from "./Chip";
import IOChip from "./IOChip";
import Pin from "./Pin";
import Wire from "./Wire";
import p5Types from "p5";
import config from "../config";
import { CircuitRenderOptions } from "../models/RenderOptions";

class Circuit {
  p5: p5Types;
  inputs: IOChip[];
  outputs: IOChip[];
  wires: Wire[];
  chips: Chip[];
  state: CircuitState;
  options: CircuitRenderOptions;
  basicGates: { [chip: string]: BasicGate };

  constructor(p5: p5Types, options: CircuitRenderOptions) {
    this.p5 = p5;
    this.inputs = [];
    this.outputs = [];
    this.wires = [];
    this.chips = [];
    this.state = {
      wiringMode: { enabled: false },
      draggingMode: { enabled: false },
    };
    this.options = options;
    // TODO: Improve definition of basic gates
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
      // A wire is not allowed to start and end on the same chip
      if (this.state.wiringMode.startPin.chip !== pin.chip) {
        this.addWire(this.state.wiringMode.startPin, pin);
      }
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
    this.p5.strokeWeight(config.component.wire.strokeWeight);
    this.p5.stroke(config.component.wire.color.stateOff);
    this.p5.line(
      this.state.wiringMode.startPin!.options.position.x,
      this.state.wiringMode.startPin!.options.position.y,
      this.p5.mouseX,
      this.p5.mouseY
    );
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
      this.wires[i].render();
    }
  }

  private renderCircuit() {
    this.p5.fill(config.component.circuit.background);
    this.p5.rect(
      this.options.position.x,
      this.options.position.y,
      this.options.size.w,
      this.options.size.h
    );
  }

  /*
  Checks if the current mouse position is over any of the specified entities
  */
  private isMouseOverlapping(entities: IOChip[]) {
    for (let i = 0; i < entities.length; i++) {
      if (this.inputs[i].isMouseOver()) {
        return true;
      }
    }
    return false;
  }

  private checkSpawnIOChip() {
    // Input
    if (
      this.p5.mouseX >= this.options.position.x - 10 &&
      this.p5.mouseX <= this.options.position.x + 10 &&
      !this.isMouseOverlapping(this.inputs)
    ) {
      this.inputs.push(
        new IOChip(this.p5, `Input_${this.inputs.length}`, true, {
          x: this.options.position.x,
          y: this.p5.mouseY,
        })
      );
    }
    // Output
    if (
      this.p5.mouseX >= this.options.position.x + this.options.size.w - 10 &&
      this.p5.mouseX <= this.options.position.x + this.options.size.w + 10 &&
      !this.isMouseOverlapping(this.outputs)
    ) {
      this.outputs.push(
        new IOChip(this.p5, `Output_${this.inputs.length}`, false, {
          x: this.options.position.x + this.options.size.w,
          y: this.p5.mouseY,
        })
      );
    }
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
    // Enforce that the startPin of the wire is an output pin
    if (startPin.isInput) {
      [startPin, endPin] = [endPin, startPin];
    }
    const wire = new Wire(this.p5, startPin, endPin);
    this.wires.push(wire);
    startPin.outgoingWires.push(wire);
  }

  execute() {
    for (let i = 0; i < this.inputs.length; i++) {
      this.inputs[i].execute();
    }
  }

  mouseClicked() {
    // TODO: Fix duplication in loops
    if (this.state.draggingMode.enabled === true) {
      return;
    }
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

    // UX
    this.checkSpawnIOChip();
  }

  mouseDragged() {
    // TODO: Fix duplication in loops
    if (this.state.wiringMode.enabled === true) {
      return;
    }
    // Input IOChips
    for (let i = 0; i < this.inputs.length; i++) {
      if (
        this.inputs[i].isMouseOver() &&
        this.state.draggingMode.enabled === false
      ) {
        this.state.draggingMode = {
          enabled: true,
          chip: this.inputs[i],
        };
      }
    }
    // Output IOChips
    for (let i = 0; i < this.outputs.length; i++) {
      if (
        this.outputs[i].isMouseOver() &&
        this.state.draggingMode.enabled === false
      ) {
        this.state.draggingMode = {
          enabled: true,
          chip: this.outputs[i],
        };
      }
    }
    // Chips
    for (let i = 0; i < this.chips.length; i++) {
      if (
        this.chips[i].isMouseOver() &&
        this.state.draggingMode.enabled === false
      ) {
        this.state.draggingMode = {
          enabled: true,
          chip: this.chips[i],
        };
      }
    }
    if (this.state.draggingMode.enabled && this.state.draggingMode.chip) {
      this.state.draggingMode.chip.mouseDragged();
    }
  }

  mouseReleased() {
    this.state.draggingMode = {
      enabled: false,
      chip: undefined,
    };
  }

  mouseMoved() {}

  render() {
    this.p5.stroke(config.document.strokeColor);
    this.p5.strokeWeight(config.document.strokeWeight);
    this.renderCircuit();
    this.renderChips();
    this.renderIOChips();
    this.renderWires();
    if (this.state.wiringMode.enabled) {
      this.renderWiringModeWire();
    }
  }
}

export default Circuit;
