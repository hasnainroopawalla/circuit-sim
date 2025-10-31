import type { Chip } from "../entities/chip";
import { CompositeChip } from "../entities/composite-chip";
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

	private onSpawnChip(chipSpec: IEvents["chip.spawn"]): void {
		let chip: Chip;

		switch (chipSpec.type) {
			case "atomic":
				chip = new chipSpec.ChipClass(chipSpec);
				break;
			case "composite":
				chip = new CompositeChip(chipSpec);
				break;
		}

		this.sim.entityService.add(chip);
	}
}
