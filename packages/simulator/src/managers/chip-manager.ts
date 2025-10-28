import { Chip } from "../entities-new/chip";
import type { IEvents } from "../services/eventing-service";
import type { Simulator } from "../simulator";
import { BaseManager } from "./base-manager";

export class ChipManager extends BaseManager {
	constructor(sim: Simulator) {
		super(sim);

		this.init();
	}

	public init(): void {
		this.sim.on("chip.spawn", (chipSpec) => this.onSpawnChip(chipSpec));
	}

	public onSpawnChip(chipSpec: IEvents["chip.spawn"]): void {
		const chip = new Chip(chipSpec);

		this.sim.entityService.add(chip);
	}
}
