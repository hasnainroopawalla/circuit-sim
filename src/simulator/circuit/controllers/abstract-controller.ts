import type { Circuit } from "../circuit";
import type { Interaction } from "./abstract-controller.interface";

export abstract class AbstractController {
  p: p5;
  protected circuit: Circuit;

  constructor(p: p5, circuit: Circuit) {
    this.p = p;
    this.circuit = circuit;
  }

  public abstract stop(): void;

  public abstract start(interaction: Interaction): void;
}
