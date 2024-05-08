import p5 from "p5";
import { AbstractRenderer, Size, State } from "../common";
import { Wire } from "./wire";
import { wireConfig } from "./wire.config";

export class WireRenderer extends AbstractRenderer<Size<"rect">> {
  wire: Wire;

  constructor(p: p5, wire: Wire) {
    super(p, { x: 0, y: 0 }, { w: 0, h: 0 });
    this.wire = wire;
  }

  public render(): void {
    this.p.push();
    this.p.strokeWeight(wireConfig.strokeWeight);
    this.p.stroke(
      this.wire.state === State.Off
        ? wireConfig.color.stateOff
        : wireConfig.color.stateOn
    );
    this.p.noFill();

    // TODO: https://github.com/hasnainroopawalla/circuit-sim/issues/15
    const tempMarkers = [
      {
        referencePoint: this.wire.startPin.renderer.position,
        waypoint: this.wire.startPin.renderer.position,
      },
      ...this.wire.markers,
    ];

    for (let i = 0; i < tempMarkers.length; i++) {
      const startPoint = tempMarkers[i].referencePoint;

      const controlPoint = {
        x: tempMarkers[i].waypoint.x,
        y: tempMarkers[i].waypoint.y,
      };

      const endPoint =
        i === tempMarkers.length - 1
          ? this.wire.endPin.renderer.position
          : tempMarkers[i + 1].referencePoint;

      this.p.bezier(
        startPoint.x,
        startPoint.y,
        controlPoint.x,
        controlPoint.y,
        controlPoint.x,
        controlPoint.y,
        endPoint.x,
        endPoint.y
      );
    }
    this.p.pop();
  }
}
