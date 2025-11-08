import type { Renderable } from "@digital-logic-sim/render-engine";
import type { Simulator } from "../../../simulator";

export type ToolArgs = { sim: Simulator };

export abstract class Tool {
	protected readonly sim: Simulator;

	constructor(args: ToolArgs) {
		this.sim = args.sim;
	}

	public abstract getRenderables(): Renderable[];

	public abstract onPointerDown(event: PointerEvent): void;

	public abstract onPointerMove(event: PointerEvent): void;
}
