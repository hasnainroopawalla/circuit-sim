import type { Renderable } from "@digital-logic-sim/render-engine";
import { BaseLayer, type BaseLayerArgs } from "../base-layer";
import type { Camera } from "./camera";
import type {
	ButtonEvent,
	KeyboardButtonType,
	MouseButtonType,
	MouseScrollType,
} from "../../input-manager";
import type { MousePosition } from "../../types";
import type { Entity } from "../../entities/entity";
import type { Pin } from "../../entities/pin";

type SimulationLayerArgs = BaseLayerArgs & {
	camera: Camera;
};

export class SimulationLayer extends BaseLayer {
	private camera: Camera;

	constructor(args: SimulationLayerArgs) {
		super(args);
		this.camera = args.camera;
	}

	public getRenderables(): Renderable[] {
		const chipRenderables: Renderable[] = this.sim.chipManager.chips.map(
			(chip) => ({
				type: "chip",
				color: chip.renderSpec.color,
				position: chip.renderSpec.position,
				label: chip.spec.name,
				inputPins: chip.inputPins.map((pin) => ({ value: pin.currentValue })),
				outputPins: chip.outputPins.map((pin) => ({ value: pin.currentValue })),
			}),
		);

		const wireRenderables: Renderable[] = this.sim.wireManager.wires.map(
			(wire) => ({
				type: "wire",
				color: wire.renderSpec.color,
				controlPoints: wire.renderSpec.controlPoints,
			}),
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
		console.log("Hovered Entity", hoveredEntity);

		if (!hoveredEntity) {
			return false;
		}

		if (hoveredEntity.type === "pin") {
			return this.activateWiringTool(hoveredEntity);
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

	public onPointerMove(event: PointerEvent): boolean {
		return false;
	}

	public onKeyboardEvent(
		event: KeyboardButtonType,
		nature: ButtonEvent,
	): boolean {
		return this.camera.onKeyboardEvent(event, nature);
	}

	private activateWiringTool(pin: Pin): boolean {
		console.log("Wiring mode triggered, startPin", pin);

		this.sim.emit("wire.spawn", {
			startPin: pin,
		});

		return true;
	}
}
