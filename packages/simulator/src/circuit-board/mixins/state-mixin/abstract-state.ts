import p5 from "p5";
import type { ICircuitBoard, MouseInput } from "../../circuit-board.interface";

export abstract class AbstractState {
  protected p: p5;
  protected circuitBoard: ICircuitBoard;

  constructor(p: p5, circuitBoard: ICircuitBoard) {
    this.p = p;
    this.circuitBoard = circuitBoard;
  }

  // TODO: rename to interact
  public abstract start(mouseInput: MouseInput): void;

  public abstract stop(): void;

  public abstract render(): void;
}
