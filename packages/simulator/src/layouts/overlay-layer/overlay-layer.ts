import type { Renderable } from "@digital-logic-sim/render-engine";
import { BaseLayer, type BaseLayerArgs } from "../base-layer";
import type {
	ButtonEvent,
	KeyboardButtonType,
	MouseButtonType,
	MouseScrollType,
} from "../../managers/input-manager";
import type { MousePosition } from "../../types";
import type { Entity } from "../../entities/entity";
import { LayoutUtils } from "../layout.utils";
import type { Camera } from "../../camera";

type OverlayLayerArgs = BaseLayerArgs;

export class OverlayLayer extends BaseLayer {
	private camera: Camera;

	constructor(args: OverlayLayerArgs) {
		super(args);
		this.camera = args.camera;
	}

	public getRenderables(renderables: Renderable[]): Renderable[] {
		// console.log(renderables);
		return [];
	}

	public onMouseButtonEvent(
		event: MouseButtonType,
		nature: ButtonEvent,
		mousePosition: MousePosition,
		hoveredEntity: Entity | null,
	): boolean {
		return false;
	}

	public onMouseScrollEvent(event: MouseScrollType): boolean {
		return false;
	}

	public onMouseMoveEvent(_mousePosition: MousePosition): boolean {
		return false;
	}

	public onKeyboardEvent(
		event: KeyboardButtonType,
		nature: ButtonEvent,
	): boolean {
		return false;
	}

	private handleLeftMouseButtonClick(hoveredEntity: Entity | null): boolean {
		return false;
	}

	private handleLeftMouseButtonPress(
		hoveredEntity: Entity | null,
		mousePosition: MousePosition,
	): boolean {
		return false;
	}

	private handleRightMouseButtonClick(
		hoveredEntity: Entity | null,
		mousePosition: MousePosition,
	): boolean {
		return false;
	}
}
