import { InteractionLayer } from "./interaction-layer";
import type { BaseLayer, BaseLayerArgs } from "./base-layer";
import { type Camera, SimulationLayer } from "./simulation-layer";
import type { Renderable } from "@digital-logic-sim/render-engine";
import type {
	MouseButtonType,
	ButtonEvent,
	MouseScrollType,
	KeyboardButtonType,
} from "../input-manager";
import type { MousePosition } from "../types";

type LayoutManagerArgs = BaseLayerArgs & {
	camera: Camera;
	screenWidth: number;
	screenHeight: number;
};

export class LayoutManager {
	private readonly layers: BaseLayer[];

	constructor(args: LayoutManagerArgs) {
		this.layers = [
			// layer 1
			new InteractionLayer(args),
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
			layer.onMouseButtonEvent(event, nature, mousePosition),
		);
	}

	public onMouseScrollEvent(event: MouseScrollType): void {
		this.layers.some((layer) => layer.onMouseScrollEvent(event));
	}

	public handleMouseHover(): void {
		// this.layers.some((layer) => layer.handleMouseHover());
	}

	public onKeyboardEvent(event: KeyboardButtonType, nature: ButtonEvent): void {
		this.layers.some((layer) => layer.onKeyboardEvent(event, nature));
	}
}
