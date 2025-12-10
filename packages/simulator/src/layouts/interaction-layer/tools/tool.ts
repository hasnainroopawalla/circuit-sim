import type { Renderable } from "@digital-logic-sim/render-engine";
import type { Simulator } from "../../../simulator";
import type {
	MouseButtonType,
	ButtonEvent,
} from "../../../managers/input-manager";
import type { MousePosition } from "../../../types";
import type { Entity } from "../../../entities/entity";

export type ToolArgs = { sim: Simulator; deactivate: () => void };

export abstract class Tool {
	protected readonly sim: ToolArgs["sim"];
	protected readonly deactivate: ToolArgs["deactivate"];

	constructor(args: ToolArgs) {
		this.sim = args.sim;
		this.deactivate = args.deactivate;
	}

	public abstract getRenderables(): Renderable[];

	public abstract onMouseMoveEvent(
		mousePosition: MousePosition,
		hoveredEntity: Entity | null,
	): void;

	public abstract onMouseButtonEvent(
		event: MouseButtonType,
		nature: ButtonEvent,
		mousePosition: MousePosition,
		hoveredEntity: Entity | null,
	): void;
}
