import {
  Interaction,
  Mode,
  RepositionMode,
  SpawnChipMode,
  WiringMode,
  CircuitRenderOptions,
  SpawnIOChipHoverMode,
} from "./circuit.interface";

import { Chip } from "./chip/chip";
import { CoreChip, CoreGate, CustomChip, IOChip, IOSlider } from "./chip";
import { Pin } from "./pin";
import { Wire, config as wireConfig } from "./wire";
import {
  EmitterEvent,
  EmitterEventArgs,
  EmitterHelper,
  emitter,
} from "../event-service";
import { CircuitHelper } from "./helpers/circuit-helper";
import { BlueprintHelper } from "./helpers/blueprint-helper";
import { idGenerator } from "./helpers/id-generator";
import { CircuitRenderer } from "./circuit.renderer";

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
  spawnIOChipHoverMode: SpawnIOChipHoverMode;
  // options: CircuitRenderOptions;
  mouseReleaseAfterDrag: boolean;

  renderer: CircuitRenderer;

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
    this.spawnIOChipHoverMode = {};
    // this.options = options;
    this.mouseReleaseAfterDrag = false;
    !isCustomChip && this.bindEventListeners();

    this.renderer = new CircuitRenderer(p5, options.position, options.size);
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

  private get isSpawnIOChipHoverMode(): boolean {
    return this.mode === Mode.SpawnIOChipHover;
  }

  private setSpawnIOChipHoverMode(type: SpawnIOChipHoverMode["type"]): void {
    this.spawnIOChipHoverMode = {
      chip: new IOChip(
        this.p,
        "ghost",
        type === "input" ? true : false,
        {
          x:
            type === "input"
              ? this.renderer.position.x
              : this.renderer.position.x + this.renderer.size.w,
          y: this.p.mouseY,
        },
        true
      ),
      type,
    };
    this.mode = Mode.SpawnIOChipHover;
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
    this.spawnIOChipHoverMode = {};
    this.mode = Mode.Idle;
  }

  private getMouseOverEntity(): IOChip | IOSlider | Pin | Chip | undefined {
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
    this.p.strokeWeight(wireConfig.strokeWeight);
    this.p.stroke(wireConfig.color.stateOff);
    this.p.noFill();

    // render initial line from startPin to either mouse position or first waypoint
    this.p.line(
      this.wiringMode.startPin.position.x,
      this.wiringMode.startPin.position.y,
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

  private renderGhostIOChip(): void {
    if (!this.spawnIOChipHoverMode.chip) {
      return;
    }
    this.spawnIOChipHoverMode.chip.mouseDragged();
    this.spawnIOChipHoverMode.chip.render();
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

  private isMouseOverInputChipPanel(): boolean {
    return this.renderer.isMouseOverInputChipPanel();
  }

  private isMouseOverOutputChipPanel(): boolean {
    return this.renderer.isMouseOverOutputChipPanel();
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
          ? this.wiringMode.startPin.position
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
            return EmitterHelper.notification(
              "Wires can only start from an output pin"
            );
          }
          this.setWiringMode({
            startPin: entity,
            markers: [],
          });
        } else if (entity instanceof IOChip) {
          entity.mouseClicked();
        } else if (entity instanceof IOSlider) {
          // TODO: Show update pin name dialog
        }
        break;

      case Interaction.Drag:
        if (entity instanceof Chip) {
          this.setRepositionMode({
            chip: entity,
          });
        } else if (entity instanceof IOSlider) {
          this.setRepositionMode({
            chip: entity.chip,
          });
        }
        break;

      case Interaction.Move:
        this.isMouseOverInputChipPanel() &&
          this.setSpawnIOChipHoverMode("input");
        this.isMouseOverOutputChipPanel() &&
          this.setSpawnIOChipHoverMode("output");
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

  private handleSpawnIOChipHoverMode(interaction: Interaction): void {
    const entity = this.getMouseOverEntity();

    switch (interaction) {
      case Interaction.Click:
        this.spawnIOChip();
        break;

      case Interaction.Move:
        if (
          entity instanceof IOSlider ||
          (!this.isMouseOverInputChipPanel() &&
            !this.isMouseOverOutputChipPanel())
        ) {
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

  private spawnIOChip() {
    this.spawnIOChipHoverMode.type === "input"
      ? this.spawnInputIOChip()
      : this.spawnOutputIOChip();
  }

  public spawnInputIOChip(): IOChip {
    const inputIOChip = new IOChip(this.p, idGenerator.inputChipId(), true, {
      x: this.renderer.position.x,
      y: this.p.mouseY,
    });
    this.inputs.push(inputIOChip);
    return inputIOChip;
  }

  public spawnOutputIOChip(): IOChip {
    const outputIOChip = new IOChip(this.p, idGenerator.outputChipId(), false, {
      x: this.renderer.position.x + this.renderer.size.w,
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
    this.isSpawnIOChipHoverMode &&
      this.handleSpawnIOChipHoverMode(Interaction.Click);
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

  public mouseMoved(): void {
    this.isIdleMode && this.handleIdleMode(Interaction.Move);
    this.isSpawnIOChipHoverMode &&
      this.handleSpawnIOChipHoverMode(Interaction.Move);
  }

  public isMouseOver(): boolean {
    return this.renderer.isMouseOver();
  }

  public saveCircuit(
    eventData: EmitterEventArgs[EmitterEvent.SaveCircuit]
  ): void {
    const { name } = eventData;

    // create the custom chip only if inputs and outputs exist
    if (this.inputs.length === 0 || this.outputs.length === 0) {
      return EmitterHelper.notification(
        "Custom chip not created due to missing inputs/outputs"
      );
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
    this.renderer.render();

    this.isWiringMode && this.renderWiringModeWire();
    this.isSpawnChipMode && this.renderSpawnChipMode();
    this.isSpawnIOChipHoverMode && this.renderGhostIOChip();

    this.renderWires();
    this.renderChips();
    this.renderIOChips();
  }
}
