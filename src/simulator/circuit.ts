import {
  Interaction,
  Mode,
  RepositionMode,
  SpawnChipsMode,
  WiringMode,
  CircuitRenderOptions,
  CustomChipBlueprint,
} from "./circuit.interface";
import type { Position } from "./shared.interface";

import { Chip } from "./chip/chip";
import { CoreChip, IOChip, CustomChip } from "./chip";
import Pin from "./pin";
import Wire from "./wire";
import config from "../config";
import { EmitterEvent, EmitterEventArgs, emitter } from "../event-service";
import { CoreGate } from "./core-gates";

class Circuit {
  // TODO: Add logger

  p: p5;
  name: string;
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

  constructor(
    p5: p5,
    name: string,
    options: CircuitRenderOptions,
    isCustomChip?: boolean
  ) {
    this.p = p5;
    this.name = name;
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
    !isCustomChip && this.bindEventListeners();
  }

  private bindEventListeners() {
    emitter.on(EmitterEvent.SpawnCoreChip, (coreChip) =>
      this.spawnCoreChip(coreChip)
    );
    emitter.on(EmitterEvent.SaveCircuit, () => this.saveCircuit());
    emitter.on(EmitterEvent.SpawnCustomChip, (customChipBlueprint) =>
      this.spawnCustomChip(customChipBlueprint)
    );
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
    this.repositionMode = {};
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

  private getPinById(pinId: string): Pin | undefined {
    for (let i = 0; i < this.inputs.length; i++) {
      const pin = this.inputs[i].getPin();
      if (pin.id === pinId) {
        return pin;
      }
    }

    for (let i = 0; i < this.outputs.length; i++) {
      const pin = this.outputs[i].getPin();
      if (pin.id === pinId) {
        return pin;
      }
    }

    for (let i = 0; i < this.chips.length; i++) {
      const pin = this.chips[i].getPinById(pinId);
      if (pin) {
        return pin;
      }
    }
  }

  public spawnCoreChip(
    eventData: EmitterEventArgs[EmitterEvent.SpawnCoreChip]
  ): void {
    const chipName = eventData.coreChip;
    const chip = new CoreChip(this.p, chipName, `chip-${this.chips.length}`);
    this.setSpawnChipsMode(chip);
    this.chips.push(chip);
  }

  public spawnCustomChip(
    eventData: EmitterEventArgs[EmitterEvent.SpawnCustomChip]
  ): void {
    const rawCircuit: CustomChipBlueprint = JSON.parse(
      eventData.customChipBlueprint
    );

    // TODO: Improve creating a new circuit
    const circuit = new Circuit(
      this.p,
      rawCircuit.name,
      {
        position: {
          x: 0,
          y: 0,
        },
        size: {
          w: 0,
          h: 0,
        },
        color: rawCircuit.color,
      },
      true
    );

    const inputs: IOChip[] = [];
    for (let i = 0; i < rawCircuit.inputs.length; i++) {
      const input = rawCircuit.inputs[i];
      inputs.push(
        new IOChip(this.p, input.id, true, {
          x: this.options.position.x,
          y: this.p.mouseY,
        })
      );
    }
    circuit.inputs = inputs;

    const outputs: IOChip[] = [];
    for (let i = 0; i < rawCircuit.outputs.length; i++) {
      const output = rawCircuit.outputs[i];
      outputs.push(
        new IOChip(this.p, output.id, false, {
          x: this.options.position.x + this.options.size.w,
          y: this.p.mouseY,
        })
      );
    }
    circuit.outputs = outputs;

    const chips: Chip[] = [];
    for (let i = 0; i < rawCircuit.chips.length; i++) {
      const chip = rawCircuit.chips[i];
      chips.push(new CoreChip(this.p, chip.coreGate, chip.id));
    }
    circuit.chips = chips;

    const wires: Wire[] = [];
    for (let i = 0; i < rawCircuit.wires.length; i++) {
      const wire = rawCircuit.wires[i];
      const startPin = circuit.getPinById(wire[0]);
      const endPin = circuit.getPinById(wire[1]);
      circuit.spawnWire(startPin, endPin, []);
    }
    circuit.wires = wires;

    const chip = new CustomChip(this.p, circuit, `chip-${this.chips.length}`);
    this.setSpawnChipsMode(chip);
    this.chips.push(chip);
  }

  public spawnInputIOChip(): void {
    this.inputs.push(
      new IOChip(this.p, `input-${this.inputs.length}`, true, {
        x: this.options.position.x,
        y: this.p.mouseY,
      })
    );
  }

  public spawnOutputIOChip(): void {
    this.outputs.push(
      new IOChip(this.p, `output-${this.outputs.length}`, false, {
        x: this.options.position.x + this.options.size.w,
        y: this.p.mouseY,
      })
    );
  }

  public spawnWire(startPin: Pin, endPin: Pin, waypoints: Position[]): void {
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
          this.spawnInputIOChip();
        } else if (this.checkSpawnOutputChip()) {
          this.spawnOutputIOChip();
          emitter.emit(EmitterEvent.Notification, {
            message: `${Math.random()}`,
          });
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
              this.spawnWire(
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

  public saveCircuit(): void {
    const inputs = this.inputs.map((input) => ({
      id: input.id,
      pin: input.pin.id,
    }));
    const outputs = this.outputs.map((output) => ({
      id: output.id,
      pin: output.pin.id,
    }));
    const chips = this.chips.map((chip) => ({
      id: chip.id,
      coreGate: chip.name as CoreGate, // TODO: no type casting
      inputPins: chip.inputPins.map((pin) => pin.id),
      outputPins: chip.outputPins.map((pin) => pin.id),
    }));
    const wires = this.wires.map((wire) => [wire.startPin.id, wire.endPin.id]);
    console.log(inputs);
    console.log(outputs);
    console.log(chips);
    console.log(wires);

    const customChip: CustomChipBlueprint = {
      name: "NAND",
      color: "green",
      inputs,
      outputs,
      chips,
      wires,
    };

    // const circuit = {
    //   name: "NAND",
    //   color: "blue",
    //   inputs: [
    //     {
    //       id: "input-0",
    //       pin: "input-0_pin-0",
    //     },
    //     {
    //       id: "input-1",
    //       pin: "input-1_pin-0",
    //     },
    //   ],

    //   outputs: [
    //     {
    //       id: "output-0",
    //       pin: "output-0_pin-0",
    //     },
    //   ],

    //   chips: [
    //     {
    //       id: "chip-0",
    //       coreGate: "AND",
    //       inputPins: ["chip-0_input-pin-0", "chip-0_input-pin-1"],
    //       outputPins: ["chip-0_output-pin-0"],
    //     },
    //     {
    //       id: "chip-1",
    //       coreGate: "NOT",
    //       inputPins: ["chip-1_input-pin-0"],
    //       outputPins: ["chip-1_output-pin-0"],
    //     },
    //   ],

    //   wires: [
    //     ["input-0_pin-0", "chip-0_input-pin-0"],
    //     ["input-1_pin-0", "chip-0_input-pin-1"],
    //     ["chip-0_output-pin-0", "chip-1_input-pin-0"],
    //     ["chip-1_output-pin-0", "output-0_pin-0"],
    //   ],
    // };
    emitter.emit(EmitterEvent.CustomChipBlueprintGenerated, {
      customChipBlueprint: JSON.stringify(customChip),
    });
  }

  public render(): void {
    this.renderCircuit();
    this.renderChips();
    this.renderIOChips();
    this.renderWires();

    this.isWiringMode() && this.renderWiringModeWire();
    this.isSpawnChipsMode() && this.renderSpawnChipMode();
  }
}

export default Circuit;
