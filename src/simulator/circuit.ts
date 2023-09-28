import type {
  DraggingMode,
  SpawnChipsMode,
  WiringMode,
} from "./circuit.interface";
import type {
  ICircuitRenderOptions,
  IPosition,
} from "./render-options.interface";

import Chip from "./chip";
import IOChip from "./io-chip";
import Pin from "./pin";
import Wire from "./wire";
import config from "../config";
// import { State } from "../enums/state";
import { CORE_GATES } from "./core-gates";

enum Mode {
  Idle = "Idle",
  Dragging = "Dragging",
  Wiring = "Wiring",
  SpawnChips = "SpawnChips",
}

class Circuit {
  p: p5;
  inputs: IOChip[];
  outputs: IOChip[];
  wires: Wire[];
  chips: Chip[];
  mode: Mode;
  wiringMode: WiringMode;
  // TODO: Change draggingMode name
  draggingMode: DraggingMode;
  spawnChipsMode: SpawnChipsMode;
  options: ICircuitRenderOptions;

  constructor(p5: p5, options: ICircuitRenderOptions) {
    this.p = p5;
    this.inputs = [];
    this.outputs = [];
    this.wires = [];
    this.chips = [];
    this.mode = Mode.Idle;
    this.wiringMode = { waypoints: [] };
    this.draggingMode = { enabled: false };
    this.spawnChipsMode = { chips: [] };
    this.options = options;
  }

  private isWiringMode(): boolean {
    return this.mode === Mode.Wiring;
  }

  private setWiringMode(): void {
    this.mode = Mode.Wiring;
  }

  private isDraggingMode(): boolean {
    return this.mode === Mode.Dragging;
  }

  private setDraggingMode(): void {
    this.mode = Mode.Dragging;
  }

  private isSpawnChipsMode(): boolean {
    return this.mode === Mode.SpawnChips;
  }

  private setSpawnChipsMode(): void {
    this.mode = Mode.SpawnChips;
  }

  private isIdleMode(): boolean {
    return this.mode === Mode.Idle;
  }

  private setIdleMode(): void {
    console.log("IDLE");
    this.spawnChipsMode = {
      chips: [],
    };
    this.draggingMode = {
      enabled: false,
      chip: undefined,
    };
    this.wiringMode = {
      startPin: undefined,
      endPin: undefined,
      waypoints: [],
    };
    this.mode = Mode.Idle;
  }

  private getClickedEntity(): IOChip | Pin | Chip | undefined {
    // Input IOChips
    for (let i = 0; i < this.inputs.length; i++) {
      const entity = this.inputs[i].isMouseOverGetEntity();
      if (entity) {
        return entity;
      }
    }
    for (let i = 0; i < this.outputs.length; i++) {
      const entity = this.outputs[i].isMouseOverGetEntity();
      if (entity) {
        return entity;
      }
    }

    // Chips
    for (let i = 0; i < this.chips.length; i++) {
      const entity = this.chips[i].isMouseOverGetEntity();
      if (entity) {
        return entity;
      }
    }
  }

  private toggleWiringMode(pin: Pin) {
    this.setWiringMode();
    this.wiringMode = {
      startPin: pin,
      waypoints: [],
    };
  }

  private renderWiringModeWire(): void {
    this.p.push();
    this.p.strokeWeight(config.component.wire.strokeWeight);
    this.p.stroke(config.document.strokeColor);
    this.p.line(
      this.wiringMode.startPin.options.position.x,
      this.wiringMode.startPin.options.position.y,
      this.p.mouseX,
      this.p.mouseY
    );
    this.p.pop();
  }

  private renderSpawnChipMode() {
    for (let i = 0; i < this.spawnChipsMode.chips.length; i++) {
      const chip = this.spawnChipsMode.chips[i];
      chip.options.position = {
        x: this.p.mouseX - chip.options.size.w / 2,
        y:
          this.p.mouseY -
          chip.options.size.h / 2 -
          (i * chip.options.size.h) / 0.8, // Extra offset for spacing between chips
      };
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
    this.p.push();
    this.p.fill(config.component.circuit.background);
    this.p.rect(
      this.options.position.x,
      this.options.position.y,
      this.options.size.w,
      this.options.size.h
    );
    this.p.pop();
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
      this.p.mouseX >= this.options.position.x - 10 &&
      this.p.mouseX <= this.options.position.x + 10 &&
      this.p.mouseY >= this.options.position.y &&
      this.p.mouseY <= this.options.position.y + this.options.size.h &&
      !this.isMouseOverlapping(this.inputs)
    ) {
      this.inputs.push(
        new IOChip(this.p, `Input_${this.inputs.length}`, true, {
          x: this.options.position.x,
          y: this.p.mouseY,
        })
      );
    }
    // Output
    if (
      this.p.mouseX >= this.options.position.x + this.options.size.w - 10 &&
      this.p.mouseX <= this.options.position.x + this.options.size.w + 10 &&
      this.p.mouseY >= this.options.position.y &&
      this.p.mouseY <= this.options.position.y + this.options.size.h &&
      !this.isMouseOverlapping(this.outputs)
    ) {
      this.outputs.push(
        new IOChip(this.p, `Output_${this.inputs.length}`, false, {
          x: this.options.position.x + this.options.size.w,
          y: this.p.mouseY,
        })
      );
    }
  }

  // private disableDraggingMode(): void {
  //   this.state.draggingMode = {
  //     enabled: false,
  //     chip: undefined,
  //   };
  // }

  // private disableSpawnChipMode(): void {
  //   this.spawnChipsMode = {
  //     enabled: false,
  //     chips: [],
  //   };
  // }

  private addWireWaypoint(x: number, y: number): void {
    this.wiringMode.waypoints.push({
      x,
      y,
    });
  }

  public addCoreChip(chipName: "AND" | "NOT" | "OR") {
    const chip = new Chip(
      this.p,
      chipName,
      CORE_GATES[chipName].inputPins,
      CORE_GATES[chipName].outputPins,
      CORE_GATES[chipName].action,
      CORE_GATES[chipName].color,
      false
    );
    this.setSpawnChipsMode();
    this.spawnChipsMode.chips.push(chip);
    this.chips.push(chip);
  }

  // public addChip(
  //   name: string,
  //   inputPins: number,
  //   outputPins: number,
  //   isCircuit: boolean,
  //   action: (inputPins: Pin[]) => State[],
  //   color: string,
  //   circuit?: Circuit
  // ) {
  //   const chip = new Chip(
  //     this.p,
  //     name,
  //     inputPins,
  //     outputPins,
  //     action,
  //     color,
  //     isCircuit,
  //     circuit
  //   );
  //   this.state.spawnChipMode = {
  //     enabled: true,
  //     chips: [...this.state.spawnChipMode.chips, chip],
  //   };
  //   this.chips.push(chip);
  // }

  public addWire(startPin: Pin, endPin: Pin, waypoints: IPosition[]) {
    // Enforce that the startPin of the wire is an output pin
    if (startPin.isInput) {
      [startPin, endPin] = [endPin, startPin];
    }
    const wire = new Wire(this.p, startPin, endPin, waypoints);
    this.wires.push(wire);
    startPin.outgoingWires.push(wire);
  }

  public execute() {
    for (let i = 0; i < this.inputs.length; i++) {
      this.inputs[i].execute();
    }
  }

  private handleClickWiringMode(): void {
    // console.log("handleClickWiringMode");
    if (this.isWiringMode()) {
      const entity = this.getClickedEntity();
      if (entity instanceof Pin) {
        // A wire is not allowed to start and end on the same chip
        if (this.wiringMode.startPin.chip !== entity.chip) {
          this.addWire(
            this.wiringMode.startPin,
            entity,
            this.wiringMode.waypoints
          );
        }
        this.setIdleMode();
      }
    }
    this.isMouseOver() && this.addWireWaypoint(this.p.mouseX, this.p.mouseY);
  }

  private handleClickSpawnChipsMode(): void {
    // console.log("handleClickSpawnChipsMode", this.spawnChipsMode);
    this.isMouseOver() && this.setIdleMode();
  }

  private handleClickIdleMode(): void {
    console.log("handleClickIdleMode");
    const entity = this.getClickedEntity();
    if (entity instanceof Pin) {
      this.toggleWiringMode(entity);
    }

    this.checkSpawnIOChip();
  }

  public mouseClicked() {
    console.log("click");
    this.isWiringMode() && this.handleClickWiringMode();
    this.isSpawnChipsMode() && this.handleClickSpawnChipsMode();
    this.isIdleMode() && this.handleClickIdleMode();
  }

  private handleDraggedWiringMode(): void {
    // console.log("handleDraggedWiringMode");
    return;
  }

  // private handleDraggedSpawnChipsMode(): void {
  // }

  private handleDraggedDraggingMode(): void {
    // console.log("handleDraggedDraggingMode");
    if (!this.isMouseOver()) {
      this.setIdleMode();
      return;
    }
    this.draggingMode.chip.mouseDragged();
  }

  private handleDraggedIdleMode(): void {
    // console.log("handleDraggedIdleMode");
    for (let i = 0; i < this.inputs.length; i++) {
      if (this.inputs[i].isMouseOver() && this.draggingMode.enabled === false) {
        this.draggingMode = {
          enabled: true,
          chip: this.inputs[i],
        };
      }
    }
    // Output IOChips
    for (let i = 0; i < this.outputs.length; i++) {
      if (
        this.outputs[i].isMouseOver() &&
        this.draggingMode.enabled === false
      ) {
        this.draggingMode = {
          enabled: true,
          chip: this.outputs[i],
        };
      }
    }
    // Chips
    for (let i = 0; i < this.chips.length; i++) {
      if (this.chips[i].isMouseOver() && this.draggingMode.enabled === false) {
        this.draggingMode = {
          enabled: true,
          chip: this.chips[i],
        };
      }
    }
  }

  public mouseDragged() {
    this.isIdleMode() && this.handleDraggedIdleMode();
    this.isWiringMode() && this.handleDraggedWiringMode();
    this.isDraggingMode() && this.handleDraggedDraggingMode();
  }

  public mouseReleased() {}

  public mouseMoved() {}

  public isMouseOver() {
    return (
      this.p.mouseX >= this.options.position.x &&
      this.p.mouseX <= this.options.position.x + this.options.size.w &&
      this.p.mouseY >= this.options.position.y &&
      this.p.mouseY <= this.options.position.y + this.options.size.h
    );
  }

  public render() {
    console.log(this.mode);
    this.renderCircuit();
    this.renderChips();
    this.renderIOChips();
    this.renderWires();

    this.isWiringMode() && this.renderWiringModeWire();
    this.isSpawnChipsMode() && this.renderSpawnChipMode();
  }
}

export default Circuit;
