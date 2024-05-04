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
import { IdleModeController } from "./controllers/idle-mode-controller";

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

  idleModeController: IdleModeController;
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

    this.idleModeController = new IdleModeController(p5, this);
    this.chipSpawnController = new ChipSpawnController(p5, this);
    this.repositionController = new RepositionController(p5, this);
    this.wiringController = new WiringController(p5, this);
    this.iOChipSpawnController = new IOChipSpawnController(p5, this);
  }

  public setMode(props: CircuitModeProps) {
    const { mode, deps } = props;
    switch (mode) {
      case Mode.Idle:
        this.chipSpawnController.clear();
        this.repositionController.clear();
        this.wiringController.clear();
        this.iOChipSpawnController.clear();
        break;
      case Mode.Reposition:
        this.repositionController.setChip(deps.chip);
        break;
      case Mode.SpawnChip:
        this.chipSpawnController.setGhostChip(deps.chip);
        break;
      case Mode.SpawnIOChipHover:
        this.iOChipSpawnController.setGhostIOChip(
          this.createIOChip(deps.kind, true, false)
        );
        break;
      case Mode.Wiring:
        this.wiringController.setStartPin(deps.startPin);
        break;
      default:
        throw new Error("Invalid mode");
    }
    this.mode = mode;
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

  public isMouseOverIOChipPanel(kind: "input" | "output"): boolean {
    return this.renderer.isMouseOverIOChipPanel(kind);
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

  // TODO: hover on IOChip bug
  public mouseClicked(): void {
    if (this.mouseReleaseAfterDrag) {
      this.mouseReleaseAfterDrag = false;
      return;
    }
    this.handleMouse(Interaction.Click);
  }

  public mouseDoubleClicked(): void {
    this.handleMouse(Interaction.DoubleClick);
  }

  public mouseDragged(): void {
    this.handleMouse(Interaction.Drag);
  }

  public mouseReleased(): void {
    if (this.mode === Mode.Reposition) {
      this.mouseReleaseAfterDrag = true;
      this.setMode({ mode: Mode.Idle });
    }
  }

  public mouseMoved(): void {
    this.handleMouse(Interaction.Move);
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

    this.mode === Mode.Wiring && this.wiringController.renderGhostWire();
    this.mode === Mode.SpawnChip && this.chipSpawnController.renderGhostChips();
    this.mode === Mode.SpawnIOChipHover &&
      this.iOChipSpawnController.renderGhostIOChip();

    this.renderWires();
    this.renderChips();
    this.renderIOChips();
  }

  // TODO: rename
  private handleMouse(interaction: Interaction): void {
    switch (this.mode) {
      case Mode.Idle:
        this.idleModeController.handle(interaction);
        break;
      case Mode.Reposition:
        this.repositionController.handle(interaction);
        break;
      case Mode.SpawnChip:
        this.chipSpawnController.handle(interaction);
        break;
      case Mode.SpawnIOChipHover:
        this.iOChipSpawnController.handle(interaction);
        break;
      case Mode.Wiring:
        this.wiringController.handle(interaction);
        break;
    }
  }

  // TODO: Move to controller
  private coreChipButtonOnClick(
    eventData: EmitterEventArgs[EmitterEvent.SpawnCoreChip]
  ): void {
    const chip = this.createCoreChip(eventData.coreChip, false);
    this.setMode({ mode: Mode.SpawnChip, deps: { chip } });
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
    this.setMode({ mode: Mode.SpawnChip, deps: { chip: customChip } });
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
}
