import type { BaseLayerArgs } from "./base-layer";
import type { CompositeLayer } from "./composite-layer";
import type { OverlayLayer } from "./overlay-layer";
import type { SimulationLayer } from "./simulation-layer";

export type LayerArgs<TLayerType extends LayerType> = Omit<
	BaseLayerArgs<TLayerType>,
	"layerType"
>;

export enum LayerType {
	Overlay,
	Simulation,
	Interaction,
	Composite,
}

export type Layer = SimulationLayer | OverlayLayer | CompositeLayer;
