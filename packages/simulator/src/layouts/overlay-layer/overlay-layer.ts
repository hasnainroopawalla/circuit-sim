import type { Renderable } from "@digital-logic-sim/render-engine";
import { BaseLayer } from "../base-layer";
import type {
	ButtonEvent,
	KeyboardButtonType,
	MouseButtonType,
	MouseScrollType,
} from "../../managers/input-manager";
import type { MousePosition } from "../../types";
import type { Entity } from "../../entities/entity";
import type { Camera } from "../../camera";
import { LayerType, type LayerArgs } from "../layout.interface";

export class OverlayLayer extends BaseLayer<LayerType.Overlay> {
	private camera: Camera;

	constructor(args: LayerArgs<LayerType.Overlay>) {
		super({
			...args,
			layerType: LayerType.Overlay,
		});
		this.camera = args.camera;
	}

	public getRenderables(_renderables: Renderable[]): Renderable[] {
		return [];
	}

	public onMouseButtonEvent(
		_event: MouseButtonType,
		_nature: ButtonEvent,
		_mousePosition: MousePosition,
		_hoveredEntity: Entity | null,
	): boolean {
		return false;
	}

	public onMouseScrollEvent(_event: MouseScrollType): boolean {
		return false;
	}

	public onMouseMoveEvent(_mousePosition: MousePosition): boolean {
		return false;
	}

	public onKeyboardEvent(
		_event: KeyboardButtonType,
		_nature: ButtonEvent,
	): boolean {
		return false;
	}

	// private handleLeftMouseButtonClick(_hoveredEntity: Entity | null): boolean {
	// 	return false;
	// }

	// private handleLeftMouseButtonPress(
	// 	_hoveredEntity: Entity | null,
	// 	_mousePosition: MousePosition,
	// ): boolean {
	// 	return false;
	// }

	// 	private handleRightMouseButtonClick(
	// 		hoveredEntity: Entity | null,
	// 		mousePosition: MousePosition,
	// 	): boolean {
	// 		return false;
	// 	}
}
