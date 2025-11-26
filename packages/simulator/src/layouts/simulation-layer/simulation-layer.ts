import type { Renderable } from "@digital-logic-sim/render-engine";
import { BaseLayer, type BaseLayerArgs } from "../base-layer";
import type { Camera } from "./camera";

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
			}),
		);

		const wireRenderables: Renderable[] = this.sim.wireManager.wires.map(
			(wire) => ({
				type: "wire",
				color: wire.renderSpec.color,
			}),
		);

		// TODO: [optimize] new object created each frame
		return [...chipRenderables, ...wireRenderables];
	}

	public onPointerDown(event: PointerEvent): boolean {
		return false;
	}

	public onPointerMove(event: PointerEvent): boolean {
		return false;
	}

	public onKeyDown(event: KeyboardEvent): boolean {
		return this.camera.onKeyboardInputEvent(event);
	}
}
