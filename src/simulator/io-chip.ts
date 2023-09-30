import config from "../config";
import { Position, State } from "./shared.interface";
import { iOPinPosition } from "./utils";
import Pin from "./pin";
import Wire from "./wire";

type IORenderOptions = {
  position: Position;
  size: number;
};

class IOChip {
  p: p5;
  name: string;
  isInput: boolean;
  pin: Pin;
  outgoingWires: Wire[];
  options: IORenderOptions;

  constructor(p5: p5, name: string, isInput: boolean, position: Position) {
    this.p = p5;
    this.name = name;
    this.isInput = isInput;
    this.pin = new Pin(p5, name, State.Off, !isInput, this);
    this.outgoingWires = [];
    this.options = {
      position,
      size: config.component.iOChip.size,
    };
  }

  private renderChip(): void {
    this.p.push();
    this.p.strokeWeight(config.component.iOChip.strokeWeight);
    this.p.fill(
      this.pin.state === State.Off
        ? config.component.iOChip.color.stateOff
        : config.component.iOChip.color.stateOn
    );
    this.p.circle(
      this.options.position.x,
      this.options.position.y,
      this.options.size
    );
    this.p.pop();
  }

  private renderInnerWire(): void {
    this.p.push();
    this.p.stroke(config.component.iOChip.innerWire.color);
    this.p.strokeWeight(config.component.iOChip.innerWire.strokeWeight);
    this.p.line(
      this.isInput
        ? this.options.position.x + this.options.size / 2
        : this.options.position.x - this.options.size / 2,
      this.options.position.y,
      this.isInput
        ? this.options.position.x + this.options.size
        : this.options.position.x - this.options.size,
      this.options.position.y
    );
    this.p.pop();
  }

  private renderPin(): void {
    const pinPosition = iOPinPosition(
      this.options.position,
      this.options.size,
      this.isInput
    );
    this.pin.setPosition(pinPosition);
    this.pin.render();
  }

  private toggle(): void {
    this.pin.state = this.pin.state === State.Off ? State.On : State.Off;
  }

  public execute(): void {
    this.pin.propagate();
  }

  public render(): void {
    this.renderInnerWire();
    this.renderChip();
    this.renderPin();
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

export default IOChip;
