import p5 from "p5";
import { Pin } from "../../pin";
import { BaseChipRenderer } from "./base-chip.renderer";
import { Position } from "../../types";

type IBaseChipArgs = {
  p: p5;
  name: string;
  id: string;
  numInputPins: number;
  numOutputPins: number;
  color: string;
};

export abstract class BaseChip {
  p: p5;
  inputPins: Pin[] = [];
  outputPins: Pin[] = [];
  id: string;
  name: string;
  numInputPins: number;
  numOutputPins: number;

  renderer: BaseChipRenderer;

  constructor(args: IBaseChipArgs) {
    this.p = args.p;
    this.id = args.id;
    this.name = args.name;
    this.numInputPins = args.numInputPins;
    this.numOutputPins = args.numOutputPins;

    this.renderer = new BaseChipRenderer({
      p: args.p,
      baseChip: this,
      color: args.color,
    });
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
