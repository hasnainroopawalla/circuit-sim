import type { Renderable } from "@digital-logic-sim/render-engine";
import { BaseLayer, type BaseLayerArgs } from "./base-layer";

export class SimulationLayer extends BaseLayer {
	constructor(args: BaseLayerArgs) {
		super(args);
	}

	public render(): void {}

	public getRenderables(): Renderable[] {
		return [];
	}

	public onPointerMove(): void {}
}
