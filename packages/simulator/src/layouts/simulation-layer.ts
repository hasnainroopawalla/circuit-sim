import type { Renderable } from "@digital-logic-sim/render-engine";
import { BaseLayer, type BaseLayerArgs } from "./base-layer";

export class SimulationLayer extends BaseLayer {
	constructor(args: BaseLayerArgs) {
		super(args);
	}

	public render(): void {}

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

	public onPointerDown(): void {}
}
