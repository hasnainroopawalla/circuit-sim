import type { Simulator } from "../simulator";
import { SCENARIOS } from "./scenarios";

export class ScenarioLoader {
	private sim: Simulator;

	constructor(sim: Simulator) {
		this.sim = sim;
	}

	public load(scenarioName: keyof typeof SCENARIOS): void {
		this.sim.emit("sim.reset", undefined);

		const scenario = SCENARIOS[scenarioName];
		scenario(this.sim);
	}
}
