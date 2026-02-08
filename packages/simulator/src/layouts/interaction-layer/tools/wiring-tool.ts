import {
	RenderableType,
	type Renderable,
} from "@digital-logic-sim/render-engine";
import { Tool, type ToolArgs } from "./tool";
import {
	ButtonEvent,
	type KeyboardButtonType,
	type MouseButtonType,
} from "../../../managers/input-manager";
import type { MousePosition } from "../../../types";
import type { Entity } from "../../../entities/entity";
import type { Pin } from "../../../entities/pin";
import type { Position } from "@digital-logic-sim/shared-types";
import { COLORS } from "../../../services/color-service";

type WiringToolArgs = ToolArgs & {
	startPin: Pin;
};

export class WiringTool extends Tool {
	private startPin: Pin;

	private currentMousePosition: Position;

	private controlPoints: Position[];

	constructor(args: WiringToolArgs) {
		super(args);

		this.startPin = args.startPin;
		this.currentMousePosition = args.startPin.getPosition();
		this.controlPoints = [];
	}

	public getRenderables(): Renderable[] {
		return [
			{
				type: RenderableType.Wire,
				color: COLORS.Ghost,
				path: [
					this.startPin.getPosition(),
					...this.controlPoints,
					this.currentMousePosition,
				],
			},
		];
	}

	public onMouseButtonEvent(
		event: MouseButtonType,
		nature: ButtonEvent,
		mousePosition: MousePosition,
		hoveredEntity: Entity | null,
	): void {
		switch (event) {
			case "leftMouseButton":
				switch (nature) {
					case ButtonEvent.Click:
						this.handleLeftMouseButtonClick(hoveredEntity, mousePosition);
						break;
				}
		}
	}

	public onMouseMoveEvent(mousePosition: MousePosition): void {
		this.currentMousePosition = mousePosition.world;
	}

	public onKeyboardEvent(
		event: KeyboardButtonType,
		_nature: ButtonEvent,
	): void {
		switch (event) {
			case "Escape":
				this.deactivate();
				break;
		}
	}

	private handleLeftMouseButtonClick(
		hoveredEntity: Entity | null,
		mousePosition: MousePosition,
	): void {
		// add control point
		if (!hoveredEntity) {
			this.controlPoints.push(mousePosition.world);
			return;
		}

		// wiring tool only deactivates if an end pin is clicked
		if (hoveredEntity.entityType !== "pin") {
			return;
		}

		this.sim.wireManager.spawnWire(
			{
				startPin: this.startPin,
				endPin: hoveredEntity,
			} /* wire connection */,
			{
				controlPoints: this.controlPoints,
			} /* init params */,
		);

		this.deactivate();
	}
}
