import p5 from "p5";
import { State } from "../common";
import type { Pin } from "../pin";
import type { WireMarker } from "./wire.interface";
import { WireRenderer } from "./wire.renderer";

export class Wire {
  p: p5;
  startPin: Pin;
  endPin: Pin;
  state: State;
  markers: WireMarker[];

  renderer: WireRenderer;

  constructor(p: p5, startPin: Pin, endPin: Pin, markers: WireMarker[] = []) {
    this.p = p;
    this.startPin = startPin;
    this.endPin = endPin;
    this.state = State.Off;
    this.markers = markers;

    this.renderer = new WireRenderer(p, this);
  }

  public propagate(): void {
    this.state = this.startPin.state;
    this.endPin.state = this.startPin.state;
    this.endPin.chip.execute();
  }

  public render(): void {
    this.renderer.render();
  }
}
