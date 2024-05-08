import p5 from "p5";
import { Position } from "../../common";
import { Pin } from "../../pin";
import { BaseChipRenderer } from "./base-chip.renderer";

export abstract class BaseChip {
  p: p5;
  inputPins: Pin[] = [];
  outputPins: Pin[] = [];
  id: string;
  name: string;
  numInputPins: number;
  numOutputPins: number;

  renderer: BaseChipRenderer;

  constructor(
    p: p5,
    name: string,
    id: string,
    numInputPins: number,
    numOutputPins: number,
    color: string
  ) {
    this.p = p;
    this.id = id;
    this.name = name;
    this.numInputPins = numInputPins;
    this.numOutputPins = numOutputPins;

    this.renderer = new BaseChipRenderer(p, this, color);
  }

  public getPin(type: string, pinId: number): Pin | undefined {
    // TODO: convert type to "input" | "output"
    return type === "input" ? this.inputPins[pinId] : this.outputPins[pinId];
  }

  public isMouseOverGetEntity(): BaseChip | Pin | undefined {
    return this.renderer.isMouseOverGetEntity();
  }

  public mouseDragged(): void {
    this.renderer.mouseDragged();
  }

  public setPosition(position: Position) {
    this.renderer.setPosition(position);
  }

  public render(): void {
    this.renderer.render();
  }

  public abstract execute(): void;
}
