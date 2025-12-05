import { ToolManager } from "./tool-manager";
import { BaseLayer, type BaseLayerArgs } from "../base-layer";
import type { Renderable } from "@digital-logic-sim/render-engine";
import type {
	MouseButtonType,
	ButtonEvent,
	MouseScrollType,
	KeyboardButtonType,
} from "../../input-manager";
import type { MousePosition } from "../../types";
import type { Entity } from "../../entities/entity";

export class InteractionLayer extends BaseLayer {
	private readonly toolManager: ToolManager;

	constructor(args: BaseLayerArgs) {
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

	public onPointerMove(event: PointerEvent): boolean {
		return this.toolManager.onPointerMove(event);
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
