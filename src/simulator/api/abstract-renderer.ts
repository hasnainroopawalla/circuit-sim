export type Position = {
  x: number;
  y: number;
};

export type Size = {
  w: number;
  h: number;
};

// TODO: add generic for entity shape (rect, circle, etc.)
export abstract class AbstractRenderer {
  p: p5;
  position: Position;
  size: Size;

  constructor(p: p5, position: Position, size: Size) {
    this.p = p;
    this.position = position;
    this.size = size;
  }

  public abstract isMouseOver(): boolean;

  public abstract mouseDragged(): void;

  public abstract render(): void;
}
