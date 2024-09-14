import p5 from "p5";
import { Pin, PinState } from "../pin";
import { WireRenderer } from "./wire.renderer";
import { Position } from "../types";

export type WireMarker = { referencePoint: Position; waypoint: Position };

export class Wire {
  p: p5;
  startPin: Pin;
  endPin: Pin;
  state: PinState;
  markers: WireMarker[];

  renderer: WireRenderer;

  constructor(p: p5, startPin: Pin, endPin: Pin, markers: WireMarker[] = []) {
    this.p = p;
    this.startPin = startPin;
    this.endPin = endPin;
    this.state = PinState.Low;
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

  public isMouseOverGetEntity(): Wire | undefined {
    return undefined;
  }
}
