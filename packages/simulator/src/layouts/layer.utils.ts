import type { BaseLayer } from "./base-layer";
import type { CompositeLayer } from "./composite-layer";
import { LayerType } from "./layout.interface";
import type { SimulationLayer } from "./simulation-layer";

export const LayerUtils = {
	isSimulationLayer: (layer: BaseLayer<LayerType>): layer is SimulationLayer =>
		layer.layerType === LayerType.Simulation,
	isCompositeLayer: (layer: BaseLayer<LayerType>): layer is CompositeLayer =>
		layer.layerType === LayerType.Composite,
};
