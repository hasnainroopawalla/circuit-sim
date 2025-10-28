import type { Simulator } from "../simulator";

export abstract class BaseService {
	protected readonly sim: Simulator;

	constructor(sim: Simulator) {
		this.sim = sim;
	}
}
