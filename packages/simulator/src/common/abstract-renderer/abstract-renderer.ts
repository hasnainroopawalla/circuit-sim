import p5 from "p5";
import type {
  CircleSize,
  Position,
  RectSize,
} from "./abstract-renderer.interface";

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
