import type { Renderable } from "@digital-logic-sim/render-engine";
import { BaseLayer, type BaseLayerArgs } from "./base-layer";

export class SimulationLayer extends BaseLayer {
	constructor(args: BaseLayerArgs) {
		super(args);
	}

	public render(): void {}

	public getRenderables(): Renderable[] {
		// return {
		// 	entities: {
		// 		chips: this.chipManager.chips.map((chip) => ({
		// 			color: chip.renderSpec.color,
		// 			position: chip.renderSpec.position,
		// 			label: chip.spec.name,
		// 		})),
		// 		wires: this.wireManager.wires.map((wire) => ({
		// 			color: wire.renderSpec.color,
		// 		})),
		// 	},
		// };
	}

	public onPointerMove(): void {}
}
