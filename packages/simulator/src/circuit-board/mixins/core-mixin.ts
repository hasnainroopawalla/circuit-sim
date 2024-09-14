import { BaseMixin } from "power-mixin";
import type { ICircuitBoard } from "../circuit-board.interface";

export type ICoreService = {
  name: string;
  isCircuitChip: boolean;
  execute: () => void;
};

// TODO: rename?
class CoreService implements ICoreService {
  public name: string;
  public isCircuitChip: boolean;

  private circuitBoard: ICircuitBoard;

  constructor(
    circuitBoard: ICircuitBoard,
    name: string,
    isCircuitChip: boolean
  ) {
    this.circuitBoard = circuitBoard;

    this.name = name;
    this.isCircuitChip = isCircuitChip;
  }

  public execute(): void {
    for (const input of this.circuitBoard.entities.inputs) {
      input.execute();
    }
  }
}

export class CoreMixin extends BaseMixin<ICircuitBoard, ICoreService> {
  constructor(name: string, isCircuitChip: boolean) {
    super({
      methods: ["execute"],
      props: ["isCircuitChip", "name"],
      initMixin: circuitBoard =>
        new CoreService(circuitBoard, name, isCircuitChip),
    });
  }
}
