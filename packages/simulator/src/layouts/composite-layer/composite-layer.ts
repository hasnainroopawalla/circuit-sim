import { BaseLayer } from "../base-layer";
import type { Camera } from "../../camera";
import type { Renderable } from "@digital-logic-sim/render-engine";
import { LayoutUtils } from "../layout.utils";
import type { Entity } from "../../entities/entity";
import type { MousePosition } from "../../types";
import type {
	MouseButtonType,
	ButtonEvent,
	KeyboardButtonType,
	MouseScrollType,
} from "../../managers/input-manager";
import { EntityUtils } from "../../entities/utils";
import { type LayerArgs, LayerType } from "../layout.interface";

type CompositeLayerArgs = LayerArgs<LayerType.Composite> & {
	compositeId: string;
};

export class CompositeLayer extends BaseLayer<LayerType.Composite> {
	private camera: Camera;
	private chipStack: string[];
	private top: number;
	private exitLayer: boolean;

	constructor(args: CompositeLayerArgs) {
		super({
			...args,
			layerType: LayerType.Composite,
		});
		this.camera = args.camera;
		this.chipStack = [args.compositeId];
		this.top = 0;
		this.exitLayer = false;
	}

	public getRenderables(): Renderable[] {
		const chipRenderables = this.sim.chipManager
			.getInternalChips(this.chipStack[this.top])
			.map((chip) => LayoutUtils.chipToRenderable(chip));

		const wireRenderables: Renderable[] = this.sim.wireManager
			.getInternalWires(this.chipStack[this.top])
			.map((wire) => {
				return {
					type: "wire",
					color: wire.getRenderState().color,
					path: wire.getPath(),
				};
			});

		// TODO: [optimize] new object created each frame
		return [...chipRenderables, ...wireRenderables];
	}

	public onMouseMoveEvent(
		mousePosition: MousePosition,
		hoveredEntity: Entity | null,
	): boolean {
		return false;
	}
	public onMouseButtonEvent(
		event: MouseButtonType,
		nature: ButtonEvent,
		mousePosition: MousePosition,
		hoveredEntity: Entity | null,
	): boolean {
		switch (event) {
			case "rightMouseButton":
				switch (nature) {
					case "click":
						return this.handleRightMouseButtonClick(hoveredEntity);
				}
		}
		return false;
	}
	public onKeyboardEvent(
		event: KeyboardButtonType,
		nature: ButtonEvent,
	): boolean {
		switch (event) {
			case "Escape":
				if (this.top !== 0) {
					this.chipStack.pop();
					--this.top;
				} else {
					this.exitLayer = true;
				}
				return true;
			default:
				return this.camera.onKeyboardEvent(event, nature);
		}
	}
	public onMouseScrollEvent(event: MouseScrollType): boolean {
		switch (event) {
			case "scrollDown":
				return this.camera.onMouseInputEvent("scrollDown");
			case "scrollUp":
				return this.camera.onMouseInputEvent("scrollUp");
			default:
				return false;
		}
	}

	public notifyManager(): boolean {
		return this.exitLayer;
	}

	private handleRightMouseButtonClick(hoveredEntity: Entity | null): boolean {
		if (!hoveredEntity) {
			return false;
		}
		if (EntityUtils.isCompositeChip(hoveredEntity)) {
			this.chipStack.push(hoveredEntity.id);
			this.top++;
		}
		return true;
	}
}
