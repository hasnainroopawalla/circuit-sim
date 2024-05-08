import type { Circuit } from "../circuit";
import type { Interaction } from "./abstract-controller.interface";

export abstract class AbstractController {
  p: p5;
  protected circuit: Circuit;

  constructor(p: p5, circuit: Circuit) {
    this.p = p;
    this.circuit = circuit;
  }

  // TODO: rename method
  public abstract clear(): void;

  // TODO: rename method
  public abstract handle(interaction: Interaction): void;
}
