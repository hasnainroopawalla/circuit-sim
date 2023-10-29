import { Pin } from "../pin";
import { Position } from "../shared.interface";
import { IOChipRenderer, PinRenderer } from "./entities";

export class RenderEngine {
  p: p5;

  constructor(p: p5) {
    this.p = p;
  }

  public renderIOChip(
    pin: Pin,
    position: Position,
    size: number,
    isInput: boolean,
    isGhost: boolean
  ): void {
    this.p.push();
    IOChipRenderer.render(this.p, pin, position, size, isInput, isGhost);
    this.p.pop();
  }

  public renderPin(
    name: string,
    position: Position,
    size: number,
    isMouseOver: boolean,
    isInput: boolean,
    isGhost: boolean
  ): void {
    this.p.push();
    PinRenderer.render(
      this.p,
      name,
      position,
      size,
      isMouseOver,
      isInput,
      isGhost
    );
    this.p.pop();
  }
}
