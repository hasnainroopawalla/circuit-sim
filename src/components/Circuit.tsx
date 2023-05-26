import { CircuitState } from "../models/CircuitState";
import Chip from "./Chip";
import IOChip from "./IOChip";
import Pin from "./Pin";
import Wire from "./Wire";
import p5Types from "p5";
import config from "../config";
import { CircuitRenderOptions, Position } from "../models/RenderOptions";
import { State } from "../enums/State";

class Circuit {
  p5: p5Types;
  inputs: IOChip[];
  outputs: IOChip[];
  wires: Wire[];
  chips: Chip[];
  state: CircuitState;
  options: CircuitRenderOptions;

  constructor(p5: p5Types, options: CircuitRenderOptions) {
    this.p5 = p5;
    this.inputs = [];
    this.outputs = [];
    this.wires = [];
    this.chips = [];
    this.state = {
      wiringMode: { enabled: false, waypoints: [] },
      draggingMode: { enabled: false },
      spawnChipMode: { enabled: false, chips: [] },
    };
    this.options = options;
  }

  private toggleWiringMode(pin: Pin) {
    if (this.state.wiringMode.enabled && this.state.wiringMode.startPin) {
      // A wire is not allowed to start and end on the same chip
      if (this.state.wiringMode.startPin.chip !== pin.chip) {
        this.addWire(
          this.state.wiringMode.startPin,
          pin,
          this.state.wiringMode.waypoints
        );
      }
      this.state.wiringMode = {
        enabled: false,
        startPin: undefined,
        endPin: undefined,
        waypoints: [],
      };
    } else {
      this.state.wiringMode = {
        enabled: true,
        startPin: pin,
        waypoints: [],
      };
    }
  }

  private renderWiringModeWire() {
    this.p5.strokeWeight(config.component.wire.strokeWeight);
    this.p5.stroke(config.document.strokeColor);
    this.p5.line(
      this.state.wiringMode.startPin!.options.position.x,
      this.state.wiringMode.startPin!.options.position.y,
      this.p5.mouseX,
      this.p5.mouseY
    );
  }

  private renderSpawnChipMode() {
    if (this.state.spawnChipMode.enabled) {
      for (let i = 0; i < this.state.spawnChipMode.chips.length; i++) {
        const chip = this.state.spawnChipMode.chips[i];
        chip.options.position = {
          x: this.p5.mouseX - chip.options.size.w / 2,
          y:
            this.p5.mouseY -
            chip.options.size.h / 2 -
            (i * chip.options.size.h) / 0.8, // Extra offset for spacing between chips
        };
      }
      // TODO: Rectangle around spawn chips
      // this.p5.fill(255, 255, 255, 50);
      // this.p5.rect(
      //   this.p5.mouseX -
      //     this.state.spawnChipMode.chips[0].options.size.w / 2 -
      //     15,
      //   this.p5.mouseY -
      //     this.state.spawnChipMode.chips[0].options.size.h *
      //       this.state.spawnChipMode.chips.length,
      //   this.state.spawnChipMode.chips[0].options.size.w + 15,
      //   this.state.spawnChipMode.chips[0].options.size.h *
      //     this.state.spawnChipMode.chips.length
      // );
    }
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
      if (entities[i].isMouseOver()) {
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
      this.p5.mouseY >= this.options.position.y &&
      this.p5.mouseY <= this.options.position.y + this.options.size.h &&
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
      this.p5.mouseY >= this.options.position.y &&
      this.p5.mouseY <= this.options.position.y + this.options.size.h &&
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

  private disableDraggingMode() {
    this.state.draggingMode = {
      enabled: false,
      chip: undefined,
    };
  }

  private disableSpawnChipMode() {
    this.state.spawnChipMode = {
      enabled: false,
      chips: [],
    };
  }

  addChip(
    name: string,
    inputPins: number,
    outputPins: number,
    isCircuit: boolean,
    action: (inputPins: Pin[]) => State[],
    color: string,
    circuit?: Circuit
  ) {
    const chip = new Chip(
      this.p5,
      name,
      inputPins,
      outputPins,
      action,
      color,
      isCircuit,
      circuit
    );
    this.state.spawnChipMode = {
      enabled: true,
      chips: [...this.state.spawnChipMode.chips, chip],
    };
    this.chips.push(chip);
  }

  addWire(startPin: Pin, endPin: Pin, waypoints: Position[]) {
    // Enforce that the startPin of the wire is an output pin
    if (startPin.isInput) {
      [startPin, endPin] = [endPin, startPin];
    }
    const wire = new Wire(this.p5, startPin, endPin, waypoints);
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
    if (this.state.draggingMode.enabled) {
      return;
    }
    if (this.state.wiringMode.enabled) {
      if (this.isMouseOver()) {
        this.state.wiringMode.waypoints.push({
          x: this.p5.mouseX,
          y: this.p5.mouseY,
        });
      }
    }
    if (this.state.spawnChipMode.enabled) {
      if (this.isMouseOver()) {
        // Place the chips in the circuit
        this.disableSpawnChipMode();
      }
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
      // Only allow dragging of the chip within the circuit bounds
      if (!this.isMouseOver()) {
        this.disableDraggingMode();
        return;
      }
      this.state.draggingMode.chip.mouseDragged();
    }
  }

  mouseReleased() {
    this.disableDraggingMode();
  }

  mouseMoved() {}

  isMouseOver() {
    return (
      this.p5.mouseX >= this.options.position.x &&
      this.p5.mouseX <= this.options.position.x + this.options.size.w &&
      this.p5.mouseY >= this.options.position.y &&
      this.p5.mouseY <= this.options.position.y + this.options.size.h
    );
  }

  render() {
    this.renderCircuit();
    this.renderChips();
    this.renderIOChips();
    this.renderWires();
    if (this.state.wiringMode.enabled) {
      this.renderWiringModeWire();
    }
    if (this.state.spawnChipMode.enabled) {
      this.renderSpawnChipMode();
    }
  }
}

export default Circuit;
