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

	// TODO: is this needed?
	public abstract render(): void;

	public abstract getRenderables(): Renderable[];

	public abstract onPointerMove(event: PointerEvent): void;
}
