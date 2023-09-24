import config from "../config";
import { State } from "../enums/state";
import { IORenderOptions, IPosition } from "./render-options.interface";
import { computeIOPinPosition } from "../utils/Position";
import Pin from "./pin";
import Wire from "./wire";

class IOChip {
  p5: p5;
  name: string;
  isInput: boolean;
  pin: Pin;
  outgoingWires: Wire[];
  options: IORenderOptions;

  constructor(p5: p5, name: string, isInput: boolean, position: IPosition) {
    this.p5 = p5;
    this.name = name;
    this.isInput = isInput;
    this.pin = new Pin(p5, name, State.Off, !isInput, this);
    this.outgoingWires = [];
    this.options = {
      position,
      size: config.component.iOChip.size,
    };
  }

  private renderChip() {
    this.p5.strokeWeight(config.component.iOChip.strokeWeight);
    this.p5.fill(
      this.pin.state === State.Off
        ? config.component.iOChip.color.stateOff
        : config.component.iOChip.color.stateOn
    );
    this.p5.circle(
      this.options.position.x,
      this.options.position.y,
      this.options.size
    );
  }

  private renderInnerWire() {
    this.p5.stroke(config.component.iOChip.innerWire.color);
    this.p5.strokeWeight(config.component.iOChip.innerWire.strokeWeight);
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

  toggle() {
    this.pin.state = this.pin.state === State.Off ? State.On : State.Off;
  }

  execute() {
    this.pin.propagate();
  }

  render() {
    this.renderInnerWire();
    this.renderChip();
    this.renderPin();
  }

  mouseClicked() {
    if (this.isMouseOver() && this.isInput) {
      this.toggle();
    }
    if (this.pin.mouseClicked()) {
      return this.pin;
    }
  }

  isMouseOver() {
    return (
      this.p5.dist(
        this.p5.mouseX,
        this.p5.mouseY,
        this.options.position.x,
        this.options.position.y
      ) <=
      this.options.size / 2
    );
  }

  mouseDragged() {
    this.options.position = {
      x: this.p5.mouseX,
      y: this.p5.mouseY,
    };
  }
}

export default IOChip;
