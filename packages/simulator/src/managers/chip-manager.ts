import { type Chip, type ChipSpec, CompositeChip } from "../entities/chips";
import type { ChipRenderSpec } from "../entities/chips/chip.interface";
import type { Simulator } from "../simulator";
import { didAnyChange } from "../utils";
import { BaseManager } from "./base-manager";

export class ChipManager extends BaseManager {
	public readonly chips: Chip[];

	constructor(sim: Simulator) {
		super(sim);

		this.chips = [];
	}

	public executeChips(): boolean {
		return didAnyChange(this.chips, (chip) => chip.execute());
	}

	public getChipById(chipId: string): Chip | undefined {
		return this.chips.find((chip) => chip.id === chipId);
	}

	public commitAllPinValues(): boolean {
		return didAnyChange(this.chips, (chip) => chip.commitPinValues());
	}

	public spawnChip(chipSpec: ChipSpec, renderSpec: ChipRenderSpec): void {
		let chip: Chip;

		switch (chipSpec.type) {
			case "io":
				chip = new chipSpec.ChipClass(chipSpec, renderSpec);
				break;
			case "atomic":
				chip = new chipSpec.ChipClass(chipSpec, renderSpec);
				break;
			case "composite":
				chip = new CompositeChip(chipSpec, renderSpec);
				break;
		}

		this.chips.push(chip);
	}
}
