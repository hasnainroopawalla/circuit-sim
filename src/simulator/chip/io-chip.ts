import { Position, State } from "../shared.interface";

import { config } from "../../config";
import { Pin } from "../pin";
import { Wire } from "../wire";
import { RenderEngine } from "../render-engine";

type IORenderOptions = {
  position: Position;
  size: number;
};

export class IOChip {
  p: p5;
  name: string;
  id: string;
  isInput: boolean;
  isGhost: boolean;
  pin: Pin;
  outgoingWires: Wire[];
  options: IORenderOptions;
  renderEngine: RenderEngine;

  constructor(
    p5: p5,
    name: string,
    isInput: boolean,
    position: Position,
    isGhost = false
  ) {
    this.p = p5;
    this.name = name;
    this.id = name;
    this.isInput = isInput;
    this.isGhost = isGhost;
    this.pin = new Pin(p5, 0, State.Off, !isInput, this, this.isGhost);
    this.outgoingWires = [];
    this.options = {
      position,
      size: config.component.iOChip.size,
    };
    this.renderEngine = new RenderEngine(this.p);
  }

  private toggle(): void {
    this.pin.state = this.pin.state === State.Off ? State.On : State.Off;
  }

  public getPin(): Pin {
    return this.pin;
  }

  public execute(): void {
    this.pin.propagate();
  }

  public render(): void {
    this.renderEngine.renderIOChip(
      this.pin,
      this.options.position,
      this.options.size,
      this.isInput,
      this.isGhost
    );
  }

  public mouseClicked(): Pin | IOChip | undefined {
    if (this.isMouseOver() && this.isInput) {
      this.toggle();
      return this;
    }
    if (this.pin.mouseClicked()) {
      return this.pin;
    }
  }

  public isMouseOver(): boolean {
    return (
      this.p.dist(
        this.p.mouseX,
        this.p.mouseY,
        this.options.position.x,
        this.options.position.y
      ) <=
      this.options.size / 2
    );
  }

  public mouseDragged(): void {
    this.options.position = {
      x: this.options.position.x,
      y: this.p.mouseY,
    };
  }

  public isMouseOverGetEntity(): IOChip | Pin | undefined {
    if (this.pin.isMouseOver()) {
      return this.pin;
    }
    if (this.isMouseOver()) {
      return this;
    }
  }
}
