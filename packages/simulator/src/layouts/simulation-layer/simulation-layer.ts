import {
	RenderableType,
	type Renderable,
} from "@digital-logic-sim/render-engine";
import { BaseLayer } from "../base-layer";
import {
	ButtonEvent,
	type KeyboardButtonType,
	type MouseButtonType,
	type MouseScrollType,
} from "../../managers/input-manager";
import type { MousePosition } from "../../types";
import type { Entity } from "../../entities/entity";
import { LayoutUtils } from "../layout.utils";
import type { Camera } from "../../camera";
import { EntityUtils } from "../../entities/utils";
import { LayerType, type LayerArgs } from "../layout.interface";

export class SimulationLayer extends BaseLayer<LayerType.Simulation> {
	private camera: Camera;
	private selectedCompositeId: string;

	constructor(args: LayerArgs<LayerType.Simulation>) {
		super({
			...args,
			layerType: LayerType.Simulation,
		});
		this.camera = args.camera;
		this.selectedCompositeId = "";

		this.init();
	}

	public getRenderables(
		_renderables: Renderable[],
		hoveredEntityId?: string,
	): Renderable[] {
		const chipRenderables = this.sim.chipManager
			.getBoardChips()
			.map((chip) => LayoutUtils.chipToRenderable(chip, hoveredEntityId));

		const wireRenderables: Renderable[] = this.sim.wireManager
			.getBoardWires()
			.map((wire) => ({
				type: RenderableType.Wire,
				path: wire.getPath(),
				color: wire.getRenderState().color,
			}));

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
					case ButtonEvent.Click:
						return this.handleLeftMouseButtonClick(hoveredEntity);
					case ButtonEvent.Press:
						return this.handleLeftMouseButtonPress(
							hoveredEntity,
							mousePosition,
						);
				}
				break;
			case "rightMouseButton":
				switch (nature) {
					case ButtonEvent.Click:
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

	public onMouseMoveEvent(
		_mousePosition: MousePosition,
		_hoveredEntity: Entity | null,
	): boolean {
		return false;
	}

	public onKeyboardEvent(
		event: KeyboardButtonType,
		nature: ButtonEvent,
	): boolean {
		return this.camera.onKeyboardEvent(event, nature);
	}

	public notifyManager(): string {
		const compositeId = this.selectedCompositeId;
		this.selectedCompositeId = "";
		return compositeId;
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

		if (
			EntityUtils.isChip(hoveredEntity) &&
			!EntityUtils.isIOChip(hoveredEntity)
		) {
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

		if (EntityUtils.isChip(hoveredEntity)) {
			this.sim.emit("entity.secondaryAction", {
				entityId: hoveredEntity.id,
				chipType: hoveredEntity.chipType,
				entityType: hoveredEntity.entityType,
				mousePosition: mousePosition.screen,
			});
			return true;
		}

		return false;
	}

	private init(): void {
		this.sim.on("composite-chip.view", ({ compositeChipId }) => {
			this.selectedCompositeId = compositeChipId;
		});
	}
}
