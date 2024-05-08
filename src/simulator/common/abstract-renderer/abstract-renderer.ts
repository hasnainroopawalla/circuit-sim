import type {
  CircleSize,
  Position,
  RectSize,
} from "./abstract-renderer.interface";

// TODO: move out of api dir
export abstract class AbstractRenderer<T extends RectSize | CircleSize> {
  p: p5;
  position: Position;
  size: T;

  constructor(p: p5, position: Position, size: T) {
    this.p = p;
    this.position = position;
    this.size = size;
  }

  // public abstract isMouseOver(): boolean;

  // public abstract mouseDragged(): void;

  public abstract render(): void;
}
