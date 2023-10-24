import {
  Interaction,
  Mode,
  RepositionMode,
  SpawnChipMode,
  WiringMode,
  CircuitRenderOptions,
} from "./circuit.interface";

import { Chip } from "./chip/chip";
import { CoreChip, CoreGate, CustomChip, IOChip } from "./chip";
import { Pin } from "./pin";
import { Wire } from "./wire";
import { config } from "../config";
import { EmitterEvent, EmitterEventArgs, emitter } from "../event-service";
import CircuitHelper from "./helpers/circuitHelper";
import BlueprintHelper from "./helpers/blueprintHelper";
import { idGenerator } from "./helpers/idGenerator";

export class Circuit {
  p: p5;
  name: string;
  inputs: IOChip[];
  outputs: IOChip[];
  wires: Wire[];
  chips: Chip[];
  mode: Mode;
  wiringMode: WiringMode;
  repositionMode: RepositionMode;
  spawnChipMode: SpawnChipMode;
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
    this.wiringMode = { markers: [] };
    this.repositionMode = {};
    this.spawnChipMode = { chips: [] };
    this.options = options;
    this.mouseReleaseAfterDrag = false;
    !isCustomChip && this.bindEventListeners();
  }

  private bindEventListeners() {
    emitter.on(EmitterEvent.SpawnCoreChip, (eventData) =>
      this.spawnCoreChip(eventData)
    );
    emitter.on(EmitterEvent.SaveCircuit, (eventData) =>
      this.saveCircuit(eventData)
    );
    emitter.on(EmitterEvent.SpawnCustomChip, (eventData) =>
      this.spawnCustomChip(eventData)
    );
    emitter.on(EmitterEvent.ImportCustomChip, (eventData) =>
      this.importCustomChip(eventData)
    );
  }

  private clear(): void {
    this.inputs = [];
    this.outputs = [];
    this.wires = [];
    this.chips = [];
  }

  private get isWiringMode(): boolean {
    return this.mode === Mode.Wiring;
  }

  private setWiringMode(wiringMode: WiringMode): void {
    this.wiringMode = wiringMode;
    this.mode = Mode.Wiring;
  }

  private get isRepositionMode(): boolean {
    return this.mode === Mode.Reposition;
  }

  private setRepositionMode(repositionMode: RepositionMode): void {
    this.repositionMode = repositionMode;
    this.mode = Mode.Reposition;
  }

  private get isSpawnChipMode(): boolean {
    return this.mode === Mode.SpawnChip;
  }

  private setSpawnChipMode(chip: Chip): void {
    this.spawnChipMode.chips.push(chip);
    this.mode = Mode.SpawnChip;
  }

  private get isIdleMode(): boolean {
    return this.mode === Mode.Idle;
  }

  private setIdleMode(): void {
    this.spawnChipMode = {
      chips: [],
    };
    this.repositionMode = {};
    this.wiringMode = {
      startPin: undefined,
      endPin: undefined,
      markers: [],
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
    // TODO: duplicate render logic from wire.ts
    if (!this.wiringMode.startPin) {
      throw new Error("Wiring mode start pin not defined");
    }
    this.p.push();
    this.p.strokeWeight(config.component.wire.strokeWeight);
    this.p.stroke(config.component.wire.color.stateOff);
    this.p.noFill();

    // render initial line from startPin to either mouse position or first waypoint
    this.p.line(
      this.wiringMode.startPin.options.position.x,
      this.wiringMode.startPin.options.position.y,
      this.wiringMode.markers.length === 0
        ? this.p.mouseX
        : this.wiringMode.markers[0].referencePoint.x,
      this.wiringMode.markers.length === 0
        ? this.p.mouseY
        : this.wiringMode.markers[0].referencePoint.y
    );

    for (let i = 0; i < this.wiringMode.markers.length; i++) {
      const startPoint = this.wiringMode.markers[i].referencePoint;
      const controlPoint = this.wiringMode.markers[i].waypoint;

      // The end point of the wire should be the current mouse position
      const endPoint =
        i === this.wiringMode.markers.length - 1
          ? { x: this.p.mouseX, y: this.p.mouseY }
          : this.wiringMode.markers[i + 1].referencePoint;

      this.p.bezier(
        startPoint.x,
        startPoint.y,
        controlPoint.x,
        controlPoint.y,
        controlPoint.x,
        controlPoint.y,
        endPoint.x,
        endPoint.y
      );
    }
    this.p.pop();
  }

  private renderSpawnChipMode(): void {
    for (let i = 0; i < this.spawnChipMode.chips.length; i++) {
      const chip = this.spawnChipMode.chips[i];
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

  private addWireMarker(): void {
    const waypoint = {
      x: this.p.mouseX,
      y: this.p.mouseY,
    };
    this.wiringMode.markers.push({
      waypoint,
      referencePoint: CircuitHelper.computeReferencePoint(
        waypoint,
        // handle the initial scenario when there are no wire markers
        this.wiringMode.markers.length === 0 && this.wiringMode.startPin
          ? this.wiringMode.startPin.options.position
          : this.wiringMode.markers[this.wiringMode.markers.length - 1].waypoint
      ),
    });
  }

  private handleIdleMode(interaction: Interaction): void {
    const entity = this.getMouseOverEntity();

    switch (interaction) {
      case Interaction.Click:
        if (entity instanceof Pin) {
          if (entity.isInput) {
            console.log("Wires can only start from an output pin");
            return;
          }
          this.setWiringMode({
            startPin: entity,
            markers: [],
          });
        } else if (entity instanceof IOChip) {
          entity.mouseClicked();
        } else if (this.checkSpawnInputChip()) {
          this.spawnInputIOChip();
        } else if (this.checkSpawnOutputChip()) {
          this.spawnOutputIOChip();
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
    const entity = this.getMouseOverEntity();

    switch (interaction) {
      case Interaction.Click:
        // TODO: Improve logic
        if (this.isWiringMode && this.wiringMode.startPin) {
          if (entity instanceof Pin) {
            // A wire is not allowed to start and end on the same chip
            if (this.wiringMode.startPin.chip !== entity.chip) {
              this.spawnWire(
                this.wiringMode.startPin,
                entity,
                this.wiringMode.markers
              );
              this.setIdleMode();
            }
          } else {
            this.addWireMarker();
          }
        }
        break;

      case Interaction.DoubleClick:
        this.setIdleMode();
        break;

      case Interaction.Drag:
        break;
    }
  }

  private handleSpawnChipMode(interaction: Interaction): void {
    switch (interaction) {
      case Interaction.Click:
        this.isMouseOver() && this.setIdleMode();
        break;

      case Interaction.Drag:
        break;
    }
  }

  private handleRepositionMode(interaction: Interaction): void {
    switch (interaction) {
      case Interaction.Click:
        break;

      case Interaction.Drag:
        if (this.repositionMode.chip instanceof Chip && this.isMouseOver()) {
          this.repositionMode.chip.mouseDragged();
        } else if (this.repositionMode.chip instanceof IOChip) {
          this.repositionMode.chip.mouseDragged();
        } else {
          this.setIdleMode();
        }
        break;
    }
  }

  public createCoreChip(coreChip: CoreGate): CoreChip {
    const chip = new CoreChip(this.p, coreChip, idGenerator.chipId(coreChip));
    this.chips.push(chip);
    return chip;
  }

  public createCustomChip(
    circuit: Circuit,
    color: string = "green"
  ): CustomChip {
    const chip = new CustomChip(
      this.p,
      circuit,
      idGenerator.chipId(circuit.name),
      color
    );
    this.chips.push(chip);
    return chip;
  }

  public spawnCoreChip(
    eventData: EmitterEventArgs[EmitterEvent.SpawnCoreChip]
  ): void {
    const { coreChip } = eventData;
    const chip = this.createCoreChip(coreChip);
    this.setSpawnChipMode(chip);
  }

  public spawnCustomChip(
    eventData: EmitterEventArgs[EmitterEvent.SpawnCustomChip]
  ): void {
    const { name, blueprint, color } = eventData;
    const circuit = BlueprintHelper.blueprintToCircuit(
      this.p,
      name,
      blueprint,
      "main"
    );

    const customChip = this.createCustomChip(circuit, color);
    this.setSpawnChipMode(customChip);
  }

  public spawnInputIOChip(): IOChip {
    const inputIOChip = new IOChip(this.p, idGenerator.inputChipId(), true, {
      x: this.options.position.x,
      y: this.p.mouseY,
    });
    this.inputs.push(inputIOChip);
    return inputIOChip;
  }

  public spawnOutputIOChip(): IOChip {
    const outputIOChip = new IOChip(this.p, idGenerator.outputChipId(), false, {
      x: this.options.position.x + this.options.size.w,
      y: this.p.mouseY,
    });
    this.outputs.push(outputIOChip);
    return outputIOChip;
  }

  public spawnWire(
    startPin: Pin,
    endPin: Pin,
    markers: WiringMode["markers"] = []
  ): void {
    const wire = new Wire(this.p, startPin, endPin, markers);
    this.wires.push(wire);
    startPin.outgoingWires.push(wire);
  }

  public execute(): void {
    for (let i = 0; i < this.inputs.length; i++) {
      this.inputs[i].execute();
    }
  }

  public mouseClicked(): void {
    if (this.mouseReleaseAfterDrag) {
      this.mouseReleaseAfterDrag = false;
      return;
    }
    this.isIdleMode && this.handleIdleMode(Interaction.Click);
    this.isWiringMode && this.handleWiringMode(Interaction.Click);
    this.isSpawnChipMode && this.handleSpawnChipMode(Interaction.Click);
    this.isRepositionMode && this.handleRepositionMode(Interaction.Click);
  }

  public mouseDoubleClicked(): void {
    this.isIdleMode && this.handleIdleMode(Interaction.DoubleClick);
    this.isWiringMode && this.handleWiringMode(Interaction.DoubleClick);
    this.isSpawnChipMode && this.handleSpawnChipMode(Interaction.DoubleClick);
    this.isRepositionMode && this.handleRepositionMode(Interaction.DoubleClick);
  }

  public mouseDragged(): void {
    this.isIdleMode && this.handleIdleMode(Interaction.Drag);
    this.isWiringMode && this.handleWiringMode(Interaction.Drag);
    this.isSpawnChipMode && this.handleSpawnChipMode(Interaction.Drag);
    this.isRepositionMode && this.handleRepositionMode(Interaction.Drag);
  }

  public mouseReleased(): void {
    if (this.isRepositionMode) {
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

  public saveCircuit(
    eventData: EmitterEventArgs[EmitterEvent.SaveCircuit]
  ): void {
    const { name } = eventData;

    // create the custom chip only if inputs and outputs exist
    if (this.inputs.length === 0 || this.outputs.length === 0) {
      return emitter.emit(EmitterEvent.Notification, {
        message: "Custom chip not created due to missing inputs/outputs",
      });
    }

    const blueprint = BlueprintHelper.circuitToBlueprint("main", this);

    emitter.emit(EmitterEvent.AddCustomChipToToolbar, {
      name,
      blueprint: JSON.stringify(blueprint),
    });

    this.clear();
  }

  // TODO: does this method need to live here?
  public importCustomChip(
    eventData: EmitterEventArgs[EmitterEvent.ImportCustomChip]
  ): void {
    const { customChipName, blueprint } = eventData;

    emitter.emit(EmitterEvent.AddCustomChipToToolbar, {
      name: customChipName,
      blueprint,
    });
  }

  public render(): void {
    this.renderCircuit();

    this.isWiringMode && this.renderWiringModeWire();
    this.isSpawnChipMode && this.renderSpawnChipMode();

    this.renderWires();
    this.renderChips();
    this.renderIOChips();
  }
}
