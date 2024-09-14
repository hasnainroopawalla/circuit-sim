import p5 from "p5";
import { BaseMixin } from "power-mixin";
import type {
  CircuitBoardEntities,
  ICircuitBoard,
} from "../circuit-board.interface";
import { Chip, CircuitChip, CoreChip, ICoreGate, IOChip } from "../../chips";
import { Wire } from "../../wire";
import { Pin } from "../../pin";
import { entityIdService } from "../services/entity-id-service";

export type IEntityService = {
  entities: CircuitBoardEntities;
  initEntities: () => void;
  createCoreChip: (coreChip: ICoreGate, spawn?: boolean) => CoreChip;
  createIOChip: (
    kind: "input" | "output",
    isGhost?: boolean,
    spawn?: boolean
  ) => IOChip;
  createCircuitChip: (
    circuitBoard: ICircuitBoard,
    color?: string,
    spawn?: boolean
  ) => CircuitChip;
  spawnChip: (chip: Chip) => void;
  spawnIOChip: (ioChip: IOChip) => void;
  spawnWire: (startPin: Pin, endPin: Pin, markers?: Wire["markers"]) => void;
};

class EntityService implements IEntityService {
  public entities!: CircuitBoardEntities;

  private p: p5;
  private circuitBoard: ICircuitBoard;

  constructor(circuitBoard: ICircuitBoard, p: p5) {
    this.circuitBoard = circuitBoard;
    this.p = p;

    this.initEntities();
  }

  public initEntities(): void {
    this.entities = {
      inputs: [],
      outputs: [],
      wires: [],
      chips: [],
    };
  }

  public createCoreChip(coreChip: ICoreGate, spawn = true): CoreChip {
    const chip = new CoreChip(
      this.p,
      coreChip,
      entityIdService.chipId(coreChip)
    );
    spawn && this.spawnChip(chip);
    return chip;
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
            entityIdService.inputChipId(),
            true,
            {
              x: this.circuitBoard.position.x,
              y: this.p.mouseY,
            },
            isGhost
          )
        : new IOChip(
            this.p,
            entityIdService.outputChipId(),
            false,
            {
              x: this.circuitBoard.position.x + this.circuitBoard.size.w,
              y: this.p.mouseY,
            },
            isGhost
          );

    spawn && this.spawnIOChip(ioChip);
    return ioChip;
  }

  public createCircuitChip(
    circuitBoard: ICircuitBoard,
    color: string = "green",
    spawn = true
  ): CircuitChip {
    const chip = new CircuitChip(
      this.p,
      circuitBoard,
      entityIdService.chipId(circuitBoard.name),
      color
    );
    spawn && this.spawnChip(chip);
    return chip;
  }

  public spawnChip(chip: Chip): void {
    this.entities.chips.push(chip);
  }

  public spawnIOChip(ioChip: IOChip): void {
    ioChip.disableGhostMode();
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
}

export class EntityMixin extends BaseMixin<ICircuitBoard, IEntityService> {
  constructor(p: p5) {
    super({
      methods: [
        "initEntities",
        "spawnChip",
        "spawnIOChip",
        "spawnWire",
        "createCoreChip",
        "createIOChip",
        "createCircuitChip",
      ],
      props: ["entities"],
      initMixin: circuitBoard => new EntityService(circuitBoard, p),
    });
  }
}
