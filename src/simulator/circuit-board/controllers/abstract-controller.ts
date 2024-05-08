import type { CircuitBoard } from "../circuit-board";
import type { Interaction } from "./abstract-controller.interface";

export abstract class AbstractController {
  p: p5;
  protected circuitBoard: CircuitBoard;

  constructor(p: p5, circuitBoard: CircuitBoard) {
    this.p = p;
    this.circuitBoard = circuitBoard;
  }

  public abstract stop(): void;

  public abstract start(interaction: Interaction): void;
}
