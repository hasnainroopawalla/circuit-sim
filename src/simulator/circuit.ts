import {
  Interaction,
  Mode,
  RepositionMode,
  SpawnChipsMode,
  WiringMode,
  CircuitRenderOptions,
} from "./circuit.interface";
import type { Position } from "./shared.interface";

import Chip from "./chip";
import IOChip from "./io-chip";
import Pin from "./pin";
import Wire from "./wire";
import config from "../config";
import { CORE_GATES } from "./core-gates";

class Circuit {
  p: p5;
  inputs: IOChip[];
  outputs: IOChip[];
  wires: Wire[];
  chips: Chip[];
  mode: Mode;
  wiringMode: WiringMode;
  repositionMode: RepositionMode;
  spawnChipsMode: SpawnChipsMode;
  options: CircuitRenderOptions;
  mouseReleaseAfterDrag: boolean;

  constructor(p5: p5, options: CircuitRenderOptions) {
    this.p = p5;
    this.inputs = [];
    this.outputs = [];
    this.wires = [];
    this.chips = [];
    this.mode = Mode.Idle;
    this.wiringMode = { waypoints: [] };
    this.repositionMode = {};
    this.spawnChipsMode = { chips: [] };
    this.options = options;
    this.mouseReleaseAfterDrag = false;
  }

  private isWiringMode(): boolean {
    return this.mode === Mode.Wiring;
  }

  private setWiringMode(wiringMode: WiringMode): void {
    this.wiringMode = wiringMode;
    this.mode = Mode.Wiring;
  }

  private isRepositionMode(): boolean {
    return this.mode === Mode.Reposition;
  }

  private setRepositionMode(repositionMode: RepositionMode): void {
    this.repositionMode = repositionMode;
    this.mode = Mode.Reposition;
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
    this.repositionMode = {
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
    // Input Entities
    for (let i = 0; i < this.inputs.length; i++) {
      const entity = this.inputs[i].isMouseOverGetEntity();
      if (entity) {
        return entity;
      }
    }

    // Output Entities
    for (let i = 0; i < this.outputs.length; i++) {
      const entity = this.outputs[i].isMouseOverGetEntity();
      if (entity) {
        return entity;
      }
    }

    // Chip Entities
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

  private renderSpawnChipMode(): void {
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

  private renderIOChips(): void {
    for (let i = 0; i < this.inputs.length; i++) {
      this.inputs[i].render();
    }
    for (let i = 0; i < this.outputs.length; i++) {
      this.outputs[i].render();
    }
  }

  private renderChips(): void {
    for (let i = 0; i < this.chips.length; i++) {
      this.chips[i].render();
    }
  }

  private renderWires(): void {
    for (let i = 0; i < this.wires.length; i++) {
      this.wires[i].render();
    }
  }

  private renderCircuit(): void {
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

  private isMouseOverlapping(entities: IOChip[]): boolean {
    for (let i = 0; i < entities.length; i++) {
      if (entities[i].isMouseOver()) {
        return true;
      }
    }
    return false;
  }

  private checkSpawnInputChip(): boolean {
    return (
      this.p.mouseX >= this.options.position.x - 10 &&
      this.p.mouseX <= this.options.position.x + 10 &&
      this.p.mouseY >= this.options.position.y &&
      this.p.mouseY <= this.options.position.y + this.options.size.h &&
      !this.isMouseOverlapping(this.inputs)
    );
  }

  private checkSpawnOutputChip(): boolean {
    return (
      this.p.mouseX >= this.options.position.x + this.options.size.w - 10 &&
      this.p.mouseX <= this.options.position.x + this.options.size.w + 10 &&
      this.p.mouseY >= this.options.position.y &&
      this.p.mouseY <= this.options.position.y + this.options.size.h &&
      !this.isMouseOverlapping(this.outputs)
    );
  }

  private addWireWaypoint(x: number, y: number): void {
    this.wiringMode.waypoints.push({
      x,
      y,
    });
  }

  public addCoreChip(chipName: "AND" | "NOT" | "OR"): void {
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

  public addInputIOChip(): void {
    this.inputs.push(
      new IOChip(this.p, `Input_${this.inputs.length}`, true, {
        x: this.options.position.x,
        y: this.p.mouseY,
      })
    );
  }

  public addOutputIOChip(): void {
    this.outputs.push(
      new IOChip(this.p, `Output_${this.inputs.length}`, false, {
        x: this.options.position.x + this.options.size.w,
        y: this.p.mouseY,
      })
    );
  }

  public addWire(startPin: Pin, endPin: Pin, waypoints: Position[]): void {
    // Enforce that the startPin of the wire is an output pin
    if (startPin.isInput) {
      [startPin, endPin] = [endPin, startPin];
    }
    const wire = new Wire(this.p, startPin, endPin, waypoints);
    this.wires.push(wire);
    startPin.outgoingWires.push(wire);
  }

  public execute(): void {
    for (let i = 0; i < this.inputs.length; i++) {
      this.inputs[i].execute();
    }
  }

  private handleIdleMode(interaction: Interaction): void {
    if (!this.isIdleMode()) {
      return;
    }

    const entity = this.getMouseOverEntity();

    switch (interaction) {
      case Interaction.Click:
        if (entity instanceof Pin) {
          this.setWiringMode({
            startPin: entity,
            waypoints: [],
          });
        } else if (entity instanceof IOChip) {
          entity.mouseClicked();
        } else if (this.checkSpawnInputChip()) {
          this.addInputIOChip();
        } else if (this.checkSpawnOutputChip()) {
          this.addOutputIOChip();
        }
        break;

      case Interaction.Drag:
        if (entity instanceof Chip) {
          this.setRepositionMode({
            chip: entity,
          });
        } else if (entity instanceof IOChip) {
          this.setRepositionMode({
            chip: entity,
          });
        }
        break;
    }
  }

  private handleWiringMode(interaction: Interaction): void {
    if (!this.isWiringMode()) {
      return;
    }

    const entity = this.getMouseOverEntity();

    switch (interaction) {
      case Interaction.Click:
        // TODO: Improve logic
        if (this.isWiringMode() && this.wiringMode.startPin) {
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
            // TODO: Add waypoint handling
            this.setIdleMode();
          }
        }
        break;

      case Interaction.Drag:
        break;
    }
  }

  private handleSpawnChipsMode(interaction: Interaction): void {
    if (!this.isSpawnChipsMode()) {
      return;
    }

    switch (interaction) {
      case Interaction.Click:
        this.isMouseOver() && this.setIdleMode();
        break;

      case Interaction.Drag:
        break;
    }
  }

  private handleRepositionMode(interaction: Interaction): void {
    if (!this.isRepositionMode()) {
      return;
    }

    switch (interaction) {
      case Interaction.Click:
        break;

      case Interaction.Drag:
        if (this.isMouseOver()) {
          this.repositionMode.chip.mouseDragged();
        } else {
          this.setIdleMode();
        }
        break;
    }
  }

  public mouseClicked(): void {
    if (this.mouseReleaseAfterDrag) {
      this.mouseReleaseAfterDrag = false;
      return;
    }
    this.handleIdleMode(Interaction.Click);
    this.handleWiringMode(Interaction.Click);
    this.handleSpawnChipsMode(Interaction.Click);
    this.handleRepositionMode(Interaction.Drag);
  }

  public mouseDragged(): void {
    // console.log("drag", this.mode);
    this.handleIdleMode(Interaction.Drag);
    this.handleWiringMode(Interaction.Drag);
    this.handleSpawnChipsMode(Interaction.Drag);
    this.handleRepositionMode(Interaction.Drag);
  }

  public mouseReleased(): void {
    if (this.isRepositionMode()) {
      this.mouseReleaseAfterDrag = true;
      this.setIdleMode();
    }
  }

  public isMouseOver(): boolean {
    return (
      this.p.mouseX >= this.options.position.x &&
      this.p.mouseX <= this.options.position.x + this.options.size.w &&
      this.p.mouseY >= this.options.position.y &&
      this.p.mouseY <= this.options.position.y + this.options.size.h
    );
  }

  public render(): void {
    // console.log(this.mode);
    this.renderCircuit();
    this.renderChips();
    this.renderIOChips();
    this.renderWires();

    this.isWiringMode() && this.renderWiringModeWire();
    this.isSpawnChipsMode() && this.renderSpawnChipMode();
  }
}

export default Circuit;
