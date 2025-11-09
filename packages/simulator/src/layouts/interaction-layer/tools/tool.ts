import type { Renderable } from "@digital-logic-sim/render-engine";
import type { Simulator } from "../../../simulator";

export type ToolArgs = { sim: Simulator; deactivate: () => void };

export abstract class Tool {
	protected readonly sim: ToolArgs["sim"];
	protected readonly deactivate: ToolArgs["deactivate"];

	constructor(args: ToolArgs) {
		this.sim = args.sim;
		this.deactivate = args.deactivate;
	}

	public abstract getRenderables(): Renderable[];

	public abstract onPointerDown(event: PointerEvent): void;

	public abstract onPointerMove(event: PointerEvent): void;
}
