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
    this.draggingMode = {};
    this.spawnChipsMode = { chips: [] };
    this.options = options;
  }

  private isWiringMode(): boolean {
    return this.mode === Mode.Wiring;
  }

  private setWiringMode(wiringMode: WiringMode): void {
    this.wiringMode = wiringMode;
    this.mode = Mode.Wiring;
  }

  private isDraggingMode(): boolean {
    return this.mode === Mode.Dragging;
  }

  private setDraggingMode(draggingMode: DraggingMode): void {
    this.draggingMode = draggingMode;
    this.mode = Mode.Dragging;
  }

  private isSpawnChipsMode(): boolean {
    return this.mode === Mode.SpawnChips;
  }

  private setSpawnChipsMode(chip: Chip): void {
    this.spawnChipsMode.chips.push(chip);
    this.mode = Mode.SpawnChips;
  }

  private isIdleMode(): boolean {
    return this.mode === Mode.Idle;
  }

  private setIdleMode(): void {
    this.spawnChipsMode = {
      chips: [],
    };
    this.draggingMode = {
      chip: undefined,
    };
    this.wiringMode = {
      startPin: undefined,
      endPin: undefined,
      waypoints: [],
    };
    this.mode = Mode.Idle;
  }

  private getMouseOverEntity(): IOChip | Pin | Chip | undefined {
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
    this.setSpawnChipsMode(chip);
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
    if (this.isWiringMode() && this.wiringMode.startPin) {
      const entity = this.getMouseOverEntity();
      if (entity instanceof Pin) {
        // A wire is not allowed to start and end on the same chip
        if (this.wiringMode.startPin.chip !== entity.chip) {
          this.addWire(
            this.wiringMode.startPin,
            entity,
            this.wiringMode.waypoints
          );
          this.setIdleMode();
        }
      } else {
        // Disable wiring mode if end pin not selected
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
    // console.log("handleClickIdleMode");
    const entity = this.getMouseOverEntity();
    if (entity instanceof Pin) {
      this.setWiringMode({
        startPin: entity,
        waypoints: [],
      });
    } else if (entity instanceof IOChip) {
      entity.mouseClicked();
    }

    // TODO: Rename this method
    this.checkSpawnIOChip();
  }

  public mouseClicked() {
    console.log("click");
    this.isIdleMode() && this.handleClickIdleMode();
    this.isWiringMode() && this.handleClickWiringMode();
    this.isSpawnChipsMode() && this.handleClickSpawnChipsMode();
  }

  private handleDraggedWiringMode(): void {
    return;
  }

  private handleDraggedDraggingMode(): void {
    if (this.isMouseOver()) {
      this.draggingMode.chip.mouseDragged();
    } else {
      this.setIdleMode();
    }
  }

  private handleDraggedIdleMode(): void {
    const entity = this.getMouseOverEntity();
    if (entity instanceof Chip) {
      this.setDraggingMode({
        chip: entity,
      });
    }
  }

  public mouseDragged() {
    this.isIdleMode() && this.handleDraggedIdleMode();
    this.isWiringMode() && this.handleDraggedWiringMode();
    this.isDraggingMode() && this.handleDraggedDraggingMode();
  }

  public mouseReleased() {
    this.isDraggingMode() && this.setIdleMode();
  }

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
