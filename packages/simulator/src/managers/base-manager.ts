import type { Simulator } from "../simulator";

export abstract class BaseManager {
	protected readonly sim: Simulator;

	constructor(sim: Simulator) {
		this.sim = sim;
	}

	public abstract init(): void;
}
