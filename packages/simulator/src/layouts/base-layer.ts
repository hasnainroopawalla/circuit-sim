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
import type { LayerType } from "./layout.interface";
import type { Camera } from "../camera";

export type BaseLayerArgs<TLayerType extends LayerType> = {
	sim: Simulator;
	camera: Camera;
	layerType: TLayerType;
};

export abstract class BaseLayer<TLayerType extends LayerType> {
	public layerType: TLayerType;

	protected readonly sim: Simulator;

	constructor(args: BaseLayerArgs<TLayerType>) {
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

	public abstract onKeyboardEvent(
		event: KeyboardButtonType,
		nature: ButtonEvent,
	): boolean;
}
