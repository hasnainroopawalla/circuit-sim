import { Wire } from "../entities/wire";
import type { Simulator } from "../simulator";
import { BaseManager } from "./base-manager";

export class WireManager extends BaseManager {
	constructor(sim: Simulator) {
		super(sim);

		this.init();
	}

	public init(): void {}

	public spawnWire(startPinId: string, endPinId: string): void {
		const wire = new Wire({
			startPinId,
			endPinId,
		});

		this.sim.entityService.add(wire);
	}

	public removeWire(): void {}
}
