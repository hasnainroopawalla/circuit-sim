import { BaseMixin } from "power-mixin";
import type { ICircuitBoard } from "../circuit-board.interface";

export type ICore = {
  execute: () => void;
};

// TODO: rename?
class Core implements ICore {
  private circuitBoard: ICircuitBoard;

  constructor(circuitBoard: ICircuitBoard) {
    this.circuitBoard = circuitBoard;
  }

  public execute(): void {
    for (const input of this.circuitBoard.entities.inputs) {
      input.execute();
    }
  }
}

export class CoreMixin extends BaseMixin<ICircuitBoard, ICore> {
  constructor() {
    super({
      methods: ["execute"],
      props: [],
      initMixin: circuitBoard => new Core(circuitBoard),
    });
  }
}
