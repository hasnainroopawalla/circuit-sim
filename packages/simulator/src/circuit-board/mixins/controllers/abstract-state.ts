import p5 from "p5";
import type { ICircuitBoard } from "../../circuit-board-mixin";
import { Interaction } from "../mouse-input-mixin";

export abstract class AbstractState {
  protected p: p5;
  protected circuitBoard: ICircuitBoard;

  constructor(p: p5, circuitBoard: ICircuitBoard) {
    this.p = p;
    this.circuitBoard = circuitBoard;
  }

  // TODO: rename to interact
  public abstract start(interaction: Interaction): void;

  public abstract stop(): void;

  public abstract render(): void;
}
