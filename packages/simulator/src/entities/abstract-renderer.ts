import p5 from "p5";
import { RectSize, CircleSize, Position } from "./types";

export abstract class AbstractRenderer<T extends RectSize | CircleSize> {
  p: p5;
  position: Position;
  size: T;

  constructor(p: p5, position: Position, size: T) {
    this.p = p;
    this.position = position;
    this.size = size;
  }

  public abstract render(): void;
}
