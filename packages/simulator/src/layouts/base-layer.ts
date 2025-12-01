import type { Renderable } from "@digital-logic-sim/render-engine";
import type { Simulator } from "../simulator";
import type {
	ButtonEvent,
	KeyboardButtonType,
	MouseButtonType,
	MouseScrollType,
} from "../input-manager";
import type { MousePosition } from "../types";
import type { Entity } from "../entities/entity";

export type BaseLayerArgs = {
	sim: Simulator;
};

export abstract class BaseLayer {
	protected readonly sim: Simulator;

	constructor(args: BaseLayerArgs) {
		this.sim = args.sim;
	}

	public abstract getRenderables(): Renderable[];

	public abstract onPointerMove(event: PointerEvent): boolean;

	public abstract onMouseButtonEvent(
		event: MouseButtonType,
		nature: ButtonEvent,
		mousePosition: MousePosition,
		hoveredEntity: Entity | null,
	): boolean;

	public abstract onMouseScrollEvent(event: MouseScrollType): boolean;

	// keyboard
	public abstract onKeyboardEvent(
		event: KeyboardButtonType,
		nature: ButtonEvent,
	): boolean;
}
