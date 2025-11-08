import type { Renderable } from "@digital-logic-sim/render-engine";
import type { Simulator } from "../simulator";

export type BaseLayerArgs = {
	sim: Simulator;
};

export abstract class BaseLayer {
	protected readonly sim: Simulator;

	constructor(args: BaseLayerArgs) {
		this.sim = args.sim;
	}

	public abstract getRenderables(): Renderable[];

	public abstract onPointerDown(event: PointerEvent): void;

	public abstract onPointerMove(event: PointerEvent): void;
}
