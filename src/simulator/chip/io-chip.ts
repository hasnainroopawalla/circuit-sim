import { Position, State } from "../shared.interface";

import { config as sharedConfig } from "../../config";
import { Pin } from "../pin";
import { Wire } from "../wire";
import { ChipHelper } from "../helpers/chip-helper";

const config = {
  sliderColor: "#4A4A4A",
  sliderPadding: 4,
  strokeWeight: 2,
  size: 35, // diameter
  color: {
    stateOff: "#152C40",
    stateOn: "#3083DC",
  },
  innerWire: {
    color: "#121212",
    strokeWeight: 3,
  },
};

export class IOChip {
  p: p5;
  name: string;
  id: string;
  isInput: boolean;
  isGhost: boolean;
  pin: Pin;
  outgoingWires: Wire[];
  position: Position;

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
    this.position = position;
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

  private renderChip(): void {
    if (this.isGhost) {
      this.p.strokeWeight(0);
      this.p.fill(sharedConfig.ghostEntityColor);
    } else {
      this.p.strokeWeight(config.strokeWeight);
      this.p.fill(
        this.pin.state === State.Off
          ? config.color.stateOff
          : config.color.stateOn
      );
    }
    this.p.circle(this.position.x, this.position.y, config.size);
  }

  private renderInnerWire(): void {
    this.isGhost
      ? this.p.stroke(sharedConfig.ghostEntityColor)
      : this.p.stroke(config.innerWire.color);
    this.p.strokeWeight(config.innerWire.strokeWeight);
    this.p.line(
      this.isInput
        ? this.position.x + config.size / 2
        : this.position.x - config.size / 2,
      this.position.y,
      this.isInput
        ? this.position.x + config.size
        : this.position.x - config.size,
      this.position.y
    );
  }

  private renderPin(): void {
    const pinPosition = ChipHelper.iOPinPosition(
      this.position,
      config.size,
      this.isInput
    );
    this.pin.setPosition(pinPosition);
    this.pin.render();
  }

  private renderSlider(): void {
    this.p.push();
    this.p.strokeWeight(0);
    this.isGhost
      ? this.p.fill(sharedConfig.ghostEntityColor)
      : this.p.fill(config.sliderColor);
    this.isInput
      ? this.p.rect(
          config.sliderPadding,
          this.position.y - config.size / 2,
          config.size / 3,
          config.size
        )
      : this.p.rect(
          this.p.windowWidth - config.size / 3 - config.sliderPadding,
          this.position.y - config.size / 2,
          config.size / 3,
          config.size
        );
    this.p.pop();
  }

  public render(): void {
    this.p.push();
    this.renderSlider();
    this.renderInnerWire();
    this.renderChip();
    this.renderPin();
    this.p.pop();
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
        this.position.x,
        this.position.y
      ) <=
      config.size / 2
    );
  }

  public mouseDragged(): void {
    this.position = {
      x: this.position.x,
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
