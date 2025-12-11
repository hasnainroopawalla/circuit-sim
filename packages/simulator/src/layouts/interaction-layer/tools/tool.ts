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
import type { Camera } from "../../../camera";

export type ToolArgs = {
	sim: Simulator;
	camera: Camera;
	deactivate: () => void;
	mousePositionService: MousePositionService;
};

export abstract class Tool {
	protected sim: ToolArgs["sim"];
	protected camera: ToolArgs["camera"];
	protected mousePositionService: ToolArgs["mousePositionService"];
	protected deactivate: ToolArgs["deactivate"];

	constructor(args: ToolArgs) {
		this.sim = args.sim;
		this.camera = args.camera;
		this.mousePositionService = args.mousePositionService;
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
