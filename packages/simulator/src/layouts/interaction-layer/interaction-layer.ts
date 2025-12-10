import { ToolManager } from "./tool-manager";
import { BaseLayer, type BaseLayerArgs } from "../base-layer";
import type { Renderable } from "@digital-logic-sim/render-engine";
import type {
	MouseButtonType,
	ButtonEvent,
	MouseScrollType,
	KeyboardButtonType,
} from "../../managers/input-manager";
import type { MousePosition } from "../../types";
import type { Entity } from "../../entities/entity";
import type { MousePositionService } from "../../services/mouse-position-service";

type InteractionLayerArgs = BaseLayerArgs & {
	mousePositionService: MousePositionService;
};

export class InteractionLayer extends BaseLayer {
	private readonly toolManager: ToolManager;

	constructor(args: InteractionLayerArgs) {
		super(args);

		this.toolManager = new ToolManager(args);
	}

	public getRenderables(): Renderable[] {
		const tool = this.toolManager.getActiveTool();
		if (!tool) {
			return [];
		}

		return tool.getRenderables();
	}

	public onMouseButtonEvent(
		event: MouseButtonType,
		nature: ButtonEvent,
		mousePosition: MousePosition,
		hoveredEntity: Entity | null,
	): boolean {
		// return false if a tool is not active
		if (!this.toolManager.getActiveTool()) {
			return false;
		}

		this.toolManager.onMouseButtonEvent(
			event,
			nature,
			mousePosition,
			hoveredEntity,
		);

		return true;
	}

	public onMouseMoveEvent(
		mousePosition: MousePosition,
		hoveredEntity: Entity | null,
	): boolean {
		return this.toolManager.onMouseMoveEvent(mousePosition, hoveredEntity);
	}

	public onMouseScrollEvent(event: MouseScrollType): boolean {
		return false;
	}

	public onKeyboardEvent(
		event: KeyboardButtonType,
		nature: ButtonEvent,
	): boolean {
		return false;
	}
}
