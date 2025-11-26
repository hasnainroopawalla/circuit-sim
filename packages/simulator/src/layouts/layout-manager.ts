import { InteractionLayer } from "./interaction-layer";
import type { BaseLayer, BaseLayerArgs } from "./base-layer";
import { Camera, SimulationLayer } from "./simulation-layer";
import type {
	CameraEntity,
	Renderable,
} from "@digital-logic-sim/render-engine";

type LayoutManagerArgs = BaseLayerArgs & {
	screenWidth: number;
	screenHeight: number;
};

export class LayoutManager {
	private readonly layers: BaseLayer[];

	private camera: Camera;

	constructor(args: LayoutManagerArgs) {
		this.camera = new Camera();

		this.layers = [
			// layer 1
			new InteractionLayer(args),
			// layer 0
			new SimulationLayer({
				...args,
				camera: this.camera,
			}),
		];
	}

	public getRenderables(): Renderable[] {
		return this.layers.flatMap((layer) => layer.getRenderables());
	}

	public getCamera(): CameraEntity {
		return this.camera.getPosition();
	}

	public onPointerDown(event: PointerEvent): void {
		this.layers.some((layer) => layer.onPointerDown(event));
	}

	public onPointerMove(event: PointerEvent): void {
		this.layers.some((layer) => layer.onPointerMove(event));
	}

	public onKeyDown(event: KeyboardEvent): void {
		this.layers.some((layer) => layer.onKeyDown(event));
	}
}
