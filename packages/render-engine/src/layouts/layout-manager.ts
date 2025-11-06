import type { Simulator } from "@digital-logic-sim/simulator";
import type { ToolController } from "../tool-controller";
import type { BaseLayer } from "./base-layer";
import { InteractionLayer } from "./interaction-layer";
import { SimulationLayer } from "./simulation-layer";

export class LayoutManager {
	private readonly layers: BaseLayer[];

	constructor(args: {
		sim: Simulator;
		toolController: ToolController;
	}) {
		// note: dont change the order.
		this.layers = [
			new InteractionLayer({
				toolController: args.toolController,
			}),
			new SimulationLayer({
				sim: args.sim,
			}),
		];
	}

	public renderAll(): void {
		this.layers.forEach((layer) => {
			layer.render();
		});
	}
}
