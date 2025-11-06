import type { Simulator } from "@digital-logic-sim/simulator";
import { BaseLayer } from "./base-layer";

export class SimulationLayer extends BaseLayer {
	private sim: Simulator;

	constructor(args: {
		sim: Simulator;
	}) {
		super();

		this.sim = args.sim;
	}

	public render(): void {
		// this.sim.getRenderView();
	}
}
