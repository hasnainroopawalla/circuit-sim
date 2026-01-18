import type { BaseLayer } from "./base-layer";
import type { LayerType } from "./layout.interface";

type Layer = BaseLayer<LayerType>;

type LayerStackEntry = {
	layer: Layer;
	priority: number;
	active: boolean;
};

export class LayerStack {
	private layers: Map<LayerType, LayerStackEntry>;

	constructor() {
		this.layers = new Map();
	}

	public register(layerType: LayerType, layer: Layer, priority: number): void {
		this.layers.set(layerType, {
			layer,
			priority,
			active: true,
		});
	}

	public getActiveLayers(): Layer[] {
		return Array.from(this.layers.values())
			.filter((e) => e.active)
			.sort((a, b) => b.priority - a.priority)
			.map((e) => e.layer);
	}

	public activate(layerType: LayerType): void {
		const layer = this.layers.get(layerType);
		if (layer) {
			layer.active = true;
		}
	}

	public deactivate(layerType: LayerType): void {
		const layer = this.layers.get(layerType);
		if (layer) {
			layer.active = false;
		}
	}
}
