import type { Simulator } from "../simulator";
import { BaseService } from "./base-service";

export class ResetService extends BaseService {
	constructor(sim: Simulator) {
		super(sim);
	}

	public resetSimulator(): void {
		this.sim.chipManager.reset();
		this.sim.wireManager.reset();
	}
}
