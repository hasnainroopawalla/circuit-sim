import type {
	ChipRenderable,
	Renderable,
} from "@digital-logic-sim/render-engine";
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
import { EntityUtils } from "../../entities/utils";

type SimulationLayerArgs = BaseLayerArgs;

export class SimulationLayer extends BaseLayer {
	private camera: Camera;

	constructor(args: SimulationLayerArgs) {
		super(args);
		this.camera = args.camera;
	}

	public getRenderables(): Renderable[] {
		const chipRenderables = this.sim.chipManager.chips.reduce((acc, chip) => {
			// TODO: improve the logic to show chips only if parent composite is open in "View mode"
			if (chip.parentCompositeId) {
				return acc;
			}
			acc.push(LayoutUtils.chipToRenderable(chip));
			return acc;
		}, [] as ChipRenderable[]);

		const wireRenderables: Renderable[] = this.sim.wireManager.wires.map(
			(wire) => {
				return {
					type: "wire",
					color: wire.renderState.color,
					path: wire.getPath(),
				};
			},
		);

		// TODO: [optimize] new object created each frame
		return [...chipRenderables, ...wireRenderables];
	}

	public onMouseButtonEvent(
		event: MouseButtonType,
		nature: ButtonEvent,
		mousePosition: MousePosition,
		hoveredEntity: Entity | null,
	): boolean {
		switch (event) {
			case "leftMouseButton":
				switch (nature) {
					case "click":
						return this.handleLeftMouseButtonClick(hoveredEntity);
					case "press":
						return this.handleLeftMouseButtonPress(
							hoveredEntity,
							mousePosition,
						);
				}
				break;
			case "rightMouseButton":
				switch (nature) {
					case "click":
						return this.handleRightMouseButtonClick(
							hoveredEntity,
							mousePosition,
						);
				}
		}

		return false;
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

	public onMouseMoveEvent(_mousePosition: MousePosition): boolean {
		return false;
	}

	public onKeyboardEvent(
		event: KeyboardButtonType,
		nature: ButtonEvent,
	): boolean {
		return this.camera.onKeyboardEvent(event, nature);
	}

	private handleLeftMouseButtonClick(hoveredEntity: Entity | null): boolean {
		if (!hoveredEntity) {
			return false;
		}

		if (EntityUtils.isPin(hoveredEntity)) {
			this.sim.emit("wire.spawn.start", {
				startPin: hoveredEntity,
			});
			return true;
		}

		if (EntityUtils.isInputChip(hoveredEntity)) {
			hoveredEntity.toggle();
			return true;
		}

		return false;
	}

	private handleLeftMouseButtonPress(
		hoveredEntity: Entity | null,
		mousePosition: MousePosition,
	): boolean {
		if (!hoveredEntity) {
			return false;
		}

		if (EntityUtils.isChip(hoveredEntity)) {
			// TODO: consider moving to a dedicated tool to render ghost
			hoveredEntity.setPosition(mousePosition.world);
			return true;
		}

		return false;
	}

	private handleRightMouseButtonClick(
		hoveredEntity: Entity | null,
		mousePosition: MousePosition,
	): boolean {
		if (!hoveredEntity) {
			return false;
		}

		if (EntityUtils.isCompositeChip(hoveredEntity)) {
			alert(
				`View chip clicked at: ${mousePosition.screen.x}, ${mousePosition.screen.y}`,
			);
		}

		return false;
	}
}
