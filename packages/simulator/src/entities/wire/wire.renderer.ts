import type p5 from "p5";
import { AbstractRenderer } from "../abstract-renderer";
import { PinState } from "../pin";
import type { Size } from "../types";
import type { Wire } from "./wire";
import { wireConfig } from "./wire.config";

type IWireRendererArgs = {
	p: p5;
	wire: Wire;
};

export class WireRenderer extends AbstractRenderer<Size<"rect">> {
	wire: Wire;

	constructor(args: IWireRendererArgs) {
		super({ p: args.p, position: { x: 0, y: 0 }, size: { w: 0, h: 0 } });
		this.wire = args.wire;
	}

	public render(): void {
		this.p.push();
		this.p.strokeWeight(wireConfig.strokeWeight);
		this.p.stroke(
			this.wire.state === PinState.Low
				? wireConfig.color.stateOff
				: wireConfig.color.stateOn,
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
				endPoint.y,
			);
		}
		this.p.pop();
	}
}
