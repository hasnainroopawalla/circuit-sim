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

	// mouse
	// TODO: abstract these methods to an InputManager
	public abstract onPointerDown(event: PointerEvent): boolean;
	public abstract onPointerMove(event: PointerEvent): boolean;

	// keyboard
	public abstract onKeyDown(event: KeyboardEvent): boolean;
}
