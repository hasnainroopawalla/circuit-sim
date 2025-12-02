import type { Position, Renderable } from "@digital-logic-sim/render-engine";
import { Tool, type ToolArgs } from "./tool";
import type { ButtonEvent, MouseButtonType } from "../../../input-manager";
import type { MousePosition } from "../../../types";
import type { Entity } from "../../../entities/entity";
import type { Pin } from "../../../entities/pin";
import { MeshUtils } from "../../../mesh-utils";

type WiringToolArgs = ToolArgs & {
	startPin: Pin;
};

export class WiringTool extends Tool {
	private startPin: Pin;

	private controlPoints: Position[];

	constructor(args: WiringToolArgs) {
		super(args);

		this.startPin = args.startPin;
		this.controlPoints = [MeshUtils.getPinPosition(args.startPin)];
	}

	public getRenderables(): Renderable[] {
		// render ghost wire
		const controlPoints = new Float32Array(2 * this.controlPoints.length);
		for (let i = 0; i < this.controlPoints.length; ++i) {
			controlPoints.set(
				[this.controlPoints[i].x, this.controlPoints[i].y],
				i * 2,
			);
		}
		return [
			{
				type: "wire",
				color: { r: 1, g: 1, b: 0, a: 1 },
				controlPoints,
			},
		];
	}

	public onMouseButtonEvent(
		event: MouseButtonType,
		nature: ButtonEvent,
		mousePosition: MousePosition,
		hoveredEntity: Entity | null,
	): void {
		// Add control point
		if (!hoveredEntity) {
			this.controlPoints.push(mousePosition.world);
			return;
		}

		// Wiring tool only deactivates if an end pin is clicked
		if (hoveredEntity.entityType !== "pin") {
			return;
		}

		// TODO @abhishek: Wire should use Position[]
		this.controlPoints.push(MeshUtils.getPinPosition(hoveredEntity));
		const controlPoints = new Float32Array(2 * this.controlPoints.length);
		for (let i = 0; i < this.controlPoints.length; ++i) {
			controlPoints.set(
				[this.controlPoints[i].x, this.controlPoints[i].y],
				i * 2,
			);
		}

		console.log("wiring tool - deactivating");
		this.sim.wireManager.spawnWire(
			{
				startPinId: this.startPin.id,
				endPinId: hoveredEntity.id,
			},
			{
				color: { r: 0, g: 1, b: 0, a: 1 },
				controlPoints: controlPoints,
			} /* renderSpec */,
		);
		this.deactivate();
	}

	public onPointerMove(event: PointerEvent): void {
		//TODO: add subscription in input manager
	}
}
