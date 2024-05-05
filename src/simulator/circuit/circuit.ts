import {
  CircuitEntities,
  CircuitModeProps,
  Interaction,
  Mode,
} from "./circuit.interface";
import { Chip, CoreChip, CoreGate, CustomChip, IOChip } from "../chip";
import { Pin } from "../pin";
import { Wire } from "../wire";
import { EmitterEvent, EmitterEventArgs, emitter } from "../../event-service";
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
import { BlueprintService } from "./services";

export class Circuit {
  p: p5;
  name: string;
  entities: CircuitEntities;
  mode: Mode;
  mouseReleaseAfterDrag: boolean;
  isCustomChip: boolean;

  renderer: CircuitRenderer;

  idleModeController: IdleModeController;
  chipSpawnController: ChipSpawnController;
  repositionController: RepositionController;
  wiringController: WiringController;
  iOChipSpawnController: IOChipSpawnController;

  blueprintService: BlueprintService;

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
    this.isCustomChip = !!isCustomChip;
    this.mode = Mode.Idle;
    this.mouseReleaseAfterDrag = false;
    this.initEntities();

    !isCustomChip && this.bindEventListeners();

    this.renderer = new CircuitRenderer(p5, options.position, options.size);

    this.idleModeController = new IdleModeController(p5, this);
    this.chipSpawnController = new ChipSpawnController(p5, this);
    this.repositionController = new RepositionController(p5, this);
    this.wiringController = new WiringController(p5, this);
    this.iOChipSpawnController = new IOChipSpawnController(p5, this);

    this.blueprintService = new BlueprintService(p5, this);
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

  public getMouseOverEntity() {
    return this.renderer.getMouseOverEntity(this.entities);
  }

  public isMouseOverIOChipPanel(kind: "input" | "output") {
    return this.renderer.isMouseOverIOChipPanel(kind);
  }

  public isMouseOver(): boolean {
    return this.renderer.isMouseOver();
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
    this.entities.chips.push(chip);
  }

  public spawnIOChip(ioChip: IOChip) {
    ioChip.ghostToReal();
    ioChip.isInput
      ? this.entities.inputs.push(ioChip)
      : this.entities.outputs.push(ioChip);
  }

  public spawnWire(
    startPin: Pin,
    endPin: Pin,
    markers: Wire["markers"] = []
  ): void {
    const wire = new Wire(this.p, startPin, endPin, markers);
    this.entities.wires.push(wire);
    startPin.outgoingWires.push(wire);
  }

  public execute(): void {
    for (const input of this.entities.inputs) {
      input.execute();
    }
  }

  public mouseClicked(): void {
    if (this.mouseReleaseAfterDrag) {
      this.mouseReleaseAfterDrag = false;
      return;
    }
    this.handleMouseInteraction(Interaction.Click);
  }

  public mouseDoubleClicked(): void {
    this.handleMouseInteraction(Interaction.DoubleClick);
  }

  public mouseDragged(): void {
    this.handleMouseInteraction(Interaction.Drag);
  }

  public mouseReleased(): void {
    if (this.mode === Mode.Reposition) {
      this.mouseReleaseAfterDrag = true;
      this.setMode({ mode: Mode.Idle });
    }
  }

  public mouseMoved(): void {
    // TODO: temporarily disabled due to IOChip hover bug
    // this.handleMouseInteraction(Interaction.Move);
    this.mode === Mode.Idle && this.idleModeController.handle(Interaction.Move);
    this.mode === Mode.SpawnIOChipHover &&
      this.iOChipSpawnController.handle(Interaction.Move);
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

    this.renderer.renderWires(this.entities.wires);
    this.renderer.renderChips(this.entities.chips);
    this.renderer.renderIOChips(this.entities.inputs, this.entities.outputs);
  }

  public initEntities(): void {
    this.entities = {
      inputs: [],
      outputs: [],
      wires: [],
      chips: [],
    };
  }

  public customChipButtonOnClick(
    customChipCircuit: Circuit,
    color: string
  ): void {
    const customChip = this.createCustomChip(customChipCircuit, color, false);
    this.setMode({ mode: Mode.SpawnChip, deps: { chip: customChip } });
  }

  private handleMouseInteraction(interaction: Interaction): void {
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

  private bindEventListeners() {
    emitter.on(EmitterEvent.SpawnCoreChip, (eventData) =>
      this.coreChipButtonOnClick(eventData)
    );
    emitter.on(EmitterEvent.ImportCustomChip, (eventData) =>
      this.importCustomChip(eventData)
    );
  }
}
