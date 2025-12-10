import { InteractionLayer } from "./interaction-layer";
import type { BaseLayer, BaseLayerArgs } from "./base-layer";
import { SimulationLayer } from "./simulation-layer";
import type { Renderable } from "@digital-logic-sim/render-engine";
import type {
	MouseButtonType,
	ButtonEvent,
	MouseScrollType,
	KeyboardButtonType,
} from "../managers/input-manager";
import type { MousePosition } from "../types";
import type { Entity } from "../entities/entity";
import type { Camera } from "../camera";
import type { MousePositionService } from "../services/mouse-position-service";

type LayoutManagerArgs = BaseLayerArgs & {
	camera: Camera;
	mousePositionService: MousePositionService;
	screenWidth: number;
	screenHeight: number;
};

export class LayoutManager {
	private readonly layers: BaseLayer[];
	private _hoveredEntity: Entity | null;

	constructor(args: LayoutManagerArgs) {
		this._hoveredEntity = null;

		this.layers = [
			// layer 1
			new InteractionLayer({
				...args,
				mousePositionService: args.mousePositionService,
			}),
			// layer 0
			new SimulationLayer({
				...args,
				camera: args.camera,
			}),
		];
	}

	public getRenderables(): Renderable[] {
		return this.layers.flatMap((layer) => layer.getRenderables());
	}

	public onMouseButtonEvent(
		event: MouseButtonType,
		nature: ButtonEvent,
		mousePosition: MousePosition,
	): void {
		this.layers.some((layer) =>
			layer.onMouseButtonEvent(
				event,
				nature,
				mousePosition,
				this._hoveredEntity,
			),
		);
	}

	public onMouseMoveEvent(mousePosition: MousePosition): void {
		this.layers.some((layer) =>
			layer.onMouseMoveEvent(mousePosition, this._hoveredEntity),
		);
	}

	public onMouseScrollEvent(event: MouseScrollType): void {
		this.layers.some((layer) => layer.onMouseScrollEvent(event));
	}

	public onKeyboardEvent(event: KeyboardButtonType, nature: ButtonEvent): void {
		this.layers.some((layer) => layer.onKeyboardEvent(event, nature));
	}

	public get hoveredEntity(): Entity | null {
		return this._hoveredEntity;
	}

	public set hoveredEntity(value: Entity | null) {
		this._hoveredEntity = value;
	}
}
