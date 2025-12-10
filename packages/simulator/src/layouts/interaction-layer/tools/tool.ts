import type { Renderable } from "@digital-logic-sim/render-engine";
import type { Simulator } from "../../../simulator";
import type {
	MouseButtonType,
	ButtonEvent,
	KeyboardButtonType,
} from "../../../managers/input-manager";
import type { MousePosition } from "../../../types";
import type { Entity } from "../../../entities/entity";
import type { MousePositionService } from "../../../services/mouse-position-service";

export type ToolArgs = {
	sim: Simulator;
	deactivate: () => void;
	mousePositionService: MousePositionService;
};

export abstract class Tool {
	protected readonly sim: ToolArgs["sim"];
	protected readonly deactivate: ToolArgs["deactivate"];

	constructor(args: ToolArgs) {
		this.sim = args.sim;
		this.deactivate = args.deactivate;
	}

	public abstract onKeyboardEvent(
		event: KeyboardButtonType,
		nature: ButtonEvent,
	): void;

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
