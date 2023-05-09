import { State } from "../enums/state";
import { IORenderOptions, Position } from "../models/renderOptions";
import { computeIOPinPosition } from "../utils/position";
import Pin from "./pin";
import Wire from "./wire";
import p5Types from "p5";

class IOChip {
  p5: p5Types;
  name: string;
  isInput: boolean;
  pin: Pin;
  outgoingWires: Wire[];
  options: IORenderOptions;

  constructor(p5: p5Types, name: string, isInput: boolean) {
    this.p5 = p5;
    this.name = name;
    this.isInput = isInput;
    this.pin = new Pin(p5, name, State.Off, this);
    this.outgoingWires = [];
    const position: Position = {
      x: Math.random() * 250,
      y: Math.random() * 250,
    };
    this.options = {
      position,
      size: 30,
    };
  }

  private renderChip() {
    this.p5.fill(this.pin.state === State.Off ? "red" : "green");
    this.p5.circle(
      this.options.position.x,
      this.options.position.y,
      this.options.size
    );
  }

  private renderWire() {
    this.p5.line(
      this.isInput
        ? this.options.position.x + this.options.size / 2
        : this.options.position.x - this.options.size / 2,
      this.options.position.y,
      this.isInput
        ? this.options.position.x + this.options.size
        : this.options.position.x - this.options.size,
      this.options.position.y
    );
  }

  private renderPin() {
    const pinPosition = computeIOPinPosition(
      this.options.position,
      this.options.size,
      this.isInput
    );
    this.pin.setPosition(pinPosition);
    this.pin.render();
  }

  private isMouseOver() {
    return (
      this.p5.dist(
        this.p5.mouseX,
        this.p5.mouseY,
        this.options.position.x,
        this.options.position.y
      ) <= this.options.size
    );
  }

  private isClickable() {
    return this.isInput;
  }

  toggle() {
    this.pin.state = this.pin.state === State.Off ? State.On : State.Off;
  }

  execute() {
    this.pin.propagate();
  }

  render() {
    this.renderChip();
    this.renderWire();
    this.renderPin();
  }

  mouseClicked() {
    if (this.isMouseOver() && this.isClickable()) {
      this.toggle();
    }
  }
}

export default IOChip;
