import { CircuitModeProps, Interaction, Mode } from "./circuit.interface";
import {
  Chip,
  CoreChip,
  CoreGate,
  CustomChip,
  IOChip,
  IOSlider,
} from "../chip";
import { Pin } from "../pin";
import { Wire } from "../wire";
import {
  EmitterEvent,
  EmitterEventArgs,
  EmitterHelper,
  emitter,
} from "../../event-service";
import { BlueprintHelper } from "../helpers/blueprint-helper";
import { idGenerator } from "../helpers/id-generator";
import { CircuitRenderer } from "./circuit.renderer";
import {
  ChipSpawnController,
  IOChipSpawnController,
  RepositionController,
  WiringController,
} from "./controllers";
import type { Position, Size } from "../api/abstract-renderer";

export class Circuit {
  p: p5;
  name: string;
  inputs: IOChip[];
  outputs: IOChip[];
  wires: Wire[];
  chips: Chip[];
  public mode: Mode;
  mouseReleaseAfterDrag: boolean;

  renderer: CircuitRenderer;

  chipSpawnController: ChipSpawnController;
  repositionController: RepositionController;
  wiringController: WiringController;
  iOChipSpawnController: IOChipSpawnController;

  constructor(
    p5: p5,
    name: string,
    options: {
      position: Position;
      size: Size;
    },
    isCustomChip?: boolean
  ) {
    this.p = p5;
    this.name = name;
    this.inputs = [];
    this.outputs = [];
    this.wires = [];
    this.chips = [];
    this.mode = Mode.Idle;
    this.mouseReleaseAfterDrag = false;
    !isCustomChip && this.bindEventListeners();

    this.renderer = new CircuitRenderer(p5, options.position, options.size);

    this.chipSpawnController = new ChipSpawnController(p5, this);
    this.repositionController = new RepositionController(p5, this);
    this.wiringController = new WiringController(p5, this);
    this.iOChipSpawnController = new IOChipSpawnController(p5, this);
  }

  private bindEventListeners() {
    emitter.on(EmitterEvent.SpawnCoreChip, (eventData) =>
      this.coreChipButtonOnClick(eventData)
    );
    emitter.on(EmitterEvent.SaveCircuit, (eventData) =>
      this.saveCircuit(eventData)
    );
    emitter.on(EmitterEvent.SpawnCustomChip, (eventData) =>
      this.customChipButtonOnClick(eventData)
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

  private setWiringMode(startPin: Pin): void {
    this.wiringController.setStartPin(startPin);
    this.mode = Mode.Wiring;
  }

  private get isRepositionMode(): boolean {
    return this.mode === Mode.Reposition;
  }

  // TODO: switch to generic setMode<T>(mode: Mode, args) => {}
  private setRepositionMode(chip: Chip | IOChip): void {
    this.repositionController.setChip(chip);
    this.mode = Mode.Reposition;
  }

  private get isSpawnChipMode(): boolean {
    return this.mode === Mode.SpawnChip;
  }

  private setSpawnChipMode(chip: Chip): void {
    this.chipSpawnController.setGhostChip(chip);
    this.mode = Mode.SpawnChip;
  }

  private get isSpawnIOChipHoverMode(): boolean {
    return this.mode === Mode.SpawnIOChipHover;
  }

  private setSpawnIOChipHoverMode(kind: "input" | "output"): void {
    const ghostiOChip = this.createIOChip(kind, true, false);
    this.iOChipSpawnController.setGhostIOChip(ghostiOChip);
    this.mode = Mode.SpawnIOChipHover;
  }

  private get isIdleMode(): boolean {
    return this.mode === Mode.Idle;
  }

  public setIdleMode(): void {
    this.chipSpawnController.clear();
    this.repositionController.clear();
    this.wiringController.clear();
    this.iOChipSpawnController.clear();
    this.mode = Mode.Idle;
  }

  // TODO: WIP
  public setMode(props: CircuitModeProps) {
    switch (props.mode) {
      case Mode.Reposition:
        console.log(props.deps.chip);
    }
  }

  public getMouseOverEntity(): IOChip | IOSlider | Pin | Chip | undefined {
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

  public isMouseOverInputChipPanel(): boolean {
    return this.renderer.isMouseOverInputChipPanel();
  }

  public isMouseOverOutputChipPanel(): boolean {
    return this.renderer.isMouseOverOutputChipPanel();
  }

  private handleIdleMode(interaction: Interaction): void {
    this.setMode(Mode.SpawnChip, { kind: "output" });
    const entity = this.getMouseOverEntity();

    switch (interaction) {
      case Interaction.Click:
        if (entity instanceof Pin) {
          if (entity.isInput) {
            return EmitterHelper.notification(
              "Wires can only start from an output pin"
            );
          }
          this.setWiringMode(entity);
        } else if (entity instanceof IOChip) {
          entity.mouseClicked();
        } else if (entity instanceof IOSlider) {
          // TODO: Show update pin name dialog
        }
        break;

      case Interaction.Drag:
        if (entity instanceof Chip) {
          this.setRepositionMode(entity);
        } else if (entity instanceof IOSlider) {
          this.setRepositionMode(entity.chip);
        }
        break;

      case Interaction.Move:
        this.isMouseOverInputChipPanel() &&
          this.setSpawnIOChipHoverMode("input");
        this.isMouseOverOutputChipPanel() &&
          this.setSpawnIOChipHoverMode("output");
    }
  }

  public createIOChip(
    kind: "input" | "output",
    isGhost = false,
    spawn = true
  ): IOChip {
    const ioChip =
      kind === "input"
        ? new IOChip(
            this.p,
            idGenerator.inputChipId(),
            true,
            {
              x: this.renderer.position.x,
              y: this.p.mouseY,
            },
            isGhost
          )
        : new IOChip(
            this.p,
            idGenerator.outputChipId(),
            false,
            {
              x: this.renderer.position.x + this.renderer.size.w,
              y: this.p.mouseY,
            },
            isGhost
          );

    spawn && this.spawnIOChip(ioChip);
    return ioChip;
  }

  public createCoreChip(coreChip: CoreGate, spawn = true): CoreChip {
    const chip = new CoreChip(this.p, coreChip, idGenerator.chipId(coreChip));
    spawn && this.spawnChip(chip);
    return chip;
  }

  public createCustomChip(
    circuit: Circuit,
    color: string = "green",
    spawn = true
  ): CustomChip {
    const chip = new CustomChip(
      this.p,
      circuit,
      idGenerator.chipId(circuit.name),
      color
    );
    spawn && this.spawnChip(chip);
    return chip;
  }

  // TODO: Move to controller
  private coreChipButtonOnClick(
    eventData: EmitterEventArgs[EmitterEvent.SpawnCoreChip]
  ): void {
    const chip = this.createCoreChip(eventData.coreChip, false);
    this.setSpawnChipMode(chip);
  }

  private customChipButtonOnClick(
    eventData: EmitterEventArgs[EmitterEvent.SpawnCustomChip]
  ): void {
    const { name, blueprint, color } = eventData;
    const circuit = BlueprintHelper.blueprintToCircuit(
      this.p,
      name,
      blueprint,
      "main"
    );

    const customChip = this.createCustomChip(circuit, color, false);
    this.setSpawnChipMode(customChip);
  }

  public spawnChip(chip: Chip): void {
    this.chips.push(chip);
  }

  public spawnIOChip(ioChip: IOChip) {
    ioChip.ghostToReal();
    ioChip.isInput ? this.inputs.push(ioChip) : this.outputs.push(ioChip);
  }

  public spawnWire(
    startPin: Pin,
    endPin: Pin,
    markers: Wire["markers"] = []
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
    this.isWiringMode && this.wiringController.handle(Interaction.Click);
    this.isSpawnChipMode && this.chipSpawnController.handle(Interaction.Click);
    this.isRepositionMode &&
      this.repositionController.handle(Interaction.Click);
    this.isSpawnIOChipHoverMode &&
      this.iOChipSpawnController.handle(Interaction.Click);
  }

  public mouseDoubleClicked(): void {
    this.isIdleMode && this.handleIdleMode(Interaction.DoubleClick);
    this.isWiringMode && this.wiringController.handle(Interaction.DoubleClick);
    this.isSpawnChipMode &&
      this.chipSpawnController.handle(Interaction.DoubleClick);
    this.isRepositionMode &&
      this.repositionController.handle(Interaction.DoubleClick);
  }

  public mouseDragged(): void {
    this.isIdleMode && this.handleIdleMode(Interaction.Drag);
    this.isWiringMode && this.wiringController.handle(Interaction.Drag);
    this.isSpawnChipMode && this.chipSpawnController.handle(Interaction.Drag);
    this.isRepositionMode && this.repositionController.handle(Interaction.Drag);
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
      this.iOChipSpawnController.handle(Interaction.Move);
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

    this.isWiringMode && this.wiringController.renderGhostWire();
    this.isSpawnChipMode && this.chipSpawnController.renderGhostChips();
    this.isSpawnIOChipHoverMode &&
      this.iOChipSpawnController.renderGhostIOChip();

    this.renderWires();
    this.renderChips();
    this.renderIOChips();
  }
}
