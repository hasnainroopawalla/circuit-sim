import p5 from "p5";
import { State, Position } from "../../common";
import { Pin } from "../../pin";
import { Wire } from "../../wire";
import { IOSlider } from "./io-chip-slider";
import { IOChipRenderer } from "./io-chip.renderer";

// TODO: improve ghost logic
export class IOChip {
  p: p5;
  name: string;
  id: string;
  isInput: boolean;
  isGhost: boolean;
  pin: Pin;
  slider: IOSlider;
  outgoingWires: Wire[];
  renderer: IOChipRenderer;

  constructor(
    p: p5,
    name: string,
    isInput: boolean,
    position: Position,
    isGhost = false
  ) {
    this.p = p;
    this.name = name;
    this.id = name;
    this.isInput = isInput;
    this.isGhost = isGhost;
    this.outgoingWires = [];

    this.pin = new Pin(p, 0, State.Off, !isInput, this, this.isGhost);

    this.renderer = new IOChipRenderer(p, this, position);

    this.slider = new IOSlider(this.p, this);
  }

  public getPin(): Pin {
    return this.pin;
  }

  public execute(): void {
    this.pin.propagate();
  }

  public render(): void {
    this.renderer.render();
  }

  public mouseClicked(): Pin | IOChip | undefined {
    return this.renderer.mouseClicked();
  }

  public isMouseOverChip(): boolean {
    return this.renderer.isMouseOverChip();
  }

  public mouseDragged(): void {
    this.renderer.mouseDragged();
  }

  public isMouseOverGetEntity(): IOChip | IOSlider | Pin | undefined {
    return this.renderer.isMouseOverGetEntity();
  }

  // TODO: rename
  public ghostToReal() {
    this.isGhost = false;
    this.pin.isGhost = false;
  }

  public toggle(): void {
    this.pin.state = this.pin.state === State.Off ? State.On : State.Off;
  }
}
