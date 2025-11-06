import { InteractionLayer } from "./interaction-layer";
import type { BaseLayer, BaseLayerArgs } from "./base-layer";
import { SimulationLayer } from "./simulation-layer";
import type { Renderable } from "@digital-logic-sim/render-engine";

export class LayoutManager {
	private readonly layers: BaseLayer[];

	constructor(args: BaseLayerArgs) {
		// note: dont change the order.
		this.layers = [new InteractionLayer(args), new SimulationLayer(args)];
	}

	public getRenderables(): Renderable[] {
		return this.layers.map((layer) => layer.getRenderables());
	}

	public renderAll(): void {}

	public onPointerMove(event: PointerEvent): void {
		this.layers.forEach((layer) => {
			layer.onPointerMove(event);
		});
	}
}
