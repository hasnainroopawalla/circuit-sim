import p5 from "p5";
import { BaseMixin } from "power-mixin";
import { ICircuitBoard } from "../circuit-board-mixin";
import { Chip, IOChip } from "../../chips";
import { Wire } from "../../wire";
import { Pin } from "../../pin";

export type CircuitBoardEntities = {
  inputs: IOChip[];
  outputs: IOChip[];
  wires: Wire[];
  chips: Chip[];
};

export type IEntityManager = {
  entities: CircuitBoardEntities;
  spawnChip: (chip: Chip) => void;
  spawnIOChip: (ioChip: IOChip) => void;
  spawnWire: (startPin: Pin, endPin: Pin, markers: Wire["markers"]) => void;
};

class EntityManager implements IEntityManager {
  public entities: CircuitBoardEntities;

  private p: p5;
  private circuitBoard: ICircuitBoard;

  constructor(circuitBoard: ICircuitBoard, p: p5) {
    this.circuitBoard = circuitBoard;
    this.p = p;

    this.entities = {
      inputs: [],
      outputs: [],
      wires: [],
      chips: [],
    };
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

export class EntityManagerMixin extends BaseMixin<
  ICircuitBoard,
  IEntityManager
> {
  constructor(p: p5) {
    super({
      methods: ["spawnChip", "spawnIOChip"],
      props: ["entities"],
      initMixin: circuitBoard => new EntityManager(circuitBoard, p),
    });
  }
}
