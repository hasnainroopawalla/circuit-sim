import { Position, State } from "../shared.interface";

import { config as sharedConfig } from "../../config";
import { Pin } from "../pin";
import { Wire } from "../wire";
import { IOChipHelper } from "../helpers/io-chip-helper";
import { IOSlider, sliderConfig } from "./io-chip-slider";

export const iOChipConfig = {
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
  slider: IOSlider;
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
    this.slider = new IOSlider(this.p, this);
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
      this.p.strokeWeight(iOChipConfig.strokeWeight);
      this.p.fill(
        this.pin.state === State.Off
          ? iOChipConfig.color.stateOff
          : iOChipConfig.color.stateOn
      );
    }
    this.p.circle(this.position.x, this.position.y, iOChipConfig.size);
  }

  private renderInnerWire(): void {
    this.isGhost
      ? this.p.stroke(sharedConfig.ghostEntityColor)
      : this.p.stroke(iOChipConfig.innerWire.color);
    this.p.strokeWeight(iOChipConfig.innerWire.strokeWeight);
    this.p.line(
      this.isInput
        ? this.position.x + iOChipConfig.size / 2
        : this.position.x - iOChipConfig.size / 2,
      this.position.y,
      this.isInput
        ? this.position.x + iOChipConfig.size
        : this.position.x - iOChipConfig.size,
      this.position.y
    );
  }

  private renderPin(): void {
    this.pin.setPosition(
      IOChipHelper.pinPosition(this.position, iOChipConfig.size, this.isInput)
    );
    this.pin.render();
  }

  private renderSlider(): void {
    this.slider.setPosition(
      IOChipHelper.sliderPosition(
        sliderConfig.padding,
        this.position,
        iOChipConfig.size,
        this.p.windowWidth,
        this.isInput
      ).position
    );
    this.slider.render();
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
    if (this.isMouseOverChip() && this.isInput) {
      this.toggle();
      return this;
    }
    if (this.pin.mouseClicked()) {
      return this.pin;
    }
  }

  public isMouseOverChip(): boolean {
    return (
      this.p.dist(
        this.p.mouseX,
        this.p.mouseY,
        this.position.x,
        this.position.y
      ) <=
      iOChipConfig.size / 2
    );
  }

  public mouseDragged(): void {
    this.position = {
      x: this.position.x,
      y: this.p.mouseY,
    };
  }

  public isMouseOverGetEntity(): IOChip | IOSlider | Pin | undefined {
    return this.isMouseOverChip()
      ? this
      : this.pin.isMouseOver()
      ? this.pin
      : this.slider.isMouseOver()
      ? this.slider
      : undefined;
  }
}
