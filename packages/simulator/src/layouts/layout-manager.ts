import { InteractionLayer } from "./interaction-layer";
import type { BaseLayer, BaseLayerArgs } from "./base-layer";
import { SimulationLayer } from "./simulation-layer";
import type { Renderable } from "@digital-logic-sim/render-engine";

export class LayoutManager {
	private readonly layers: BaseLayer[];

	constructor(args: BaseLayerArgs) {
		this.layers = [
			// layer 1
			new InteractionLayer(args),
			// layer 0
			new SimulationLayer(args),
		];
	}

	public getRenderables(): Renderable[] {
		return this.layers.flatMap((layer) => layer.getRenderables());
	}

	// TODO @hasnain - active layer should intercept reqs
	public onPointerDown(event: PointerEvent): void {
		this.layers.forEach((layer) => {
			layer.onPointerDown(event);
		});
	}

	public onPointerMove(event: PointerEvent): void {
		this.layers.forEach((layer) => {
			layer.onPointerMove(event);
		});
	}
}
