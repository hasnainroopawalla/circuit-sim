import type { Renderable } from "@digital-logic-sim/render-engine";
import type { Simulator } from "../simulator";
import type {
	ButtonEvent,
	KeyboardButtonType,
	MouseButtonType,
	MouseScrollType,
} from "../managers/input-manager";
import type { MousePosition } from "../types";
import type { Entity } from "../entities/entity";
import type { Camera } from "../camera";

export type BaseLayerArgs = {
	sim: Simulator;
	camera: Camera;
	layerType: Layer;
};

export enum Layer {
	Overlay,
	Simulation,
	Interaction,
	Composite,
}

export abstract class BaseLayer {
	protected readonly sim: Simulator;
	protected layerType: Layer;
	constructor(args: BaseLayerArgs) {
		this.sim = args.sim;
		this.layerType = args.layerType;
	}

	public abstract getRenderables(renderables: Renderable[]): Renderable[];

	public abstract onMouseMoveEvent(
		mousePosition: MousePosition,
		hoveredEntity: Entity | null,
	): boolean;

	public abstract onMouseButtonEvent(
		event: MouseButtonType,
		nature: ButtonEvent,
		mousePosition: MousePosition,
		hoveredEntity: Entity | null,
	): boolean;

	public abstract onMouseScrollEvent(event: MouseScrollType): boolean;

	// keyboard
	public abstract onKeyboardEvent(
		event: KeyboardButtonType,
		nature: ButtonEvent,
	): boolean;

	public getLayerType(): Layer {
		return this.layerType;
	}
}
