import {
	type Chip,
	type ChipSpec,
	type ChipRenderSpec,
	type IOChipSpec,
	type IOChip,
	CompositeChip,
} from "../entities/chips";
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

		switch (chipSpec.chipType) {
			case "io":
				chip = this.createIOChip(chipSpec, renderSpec);
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

	private createIOChip(
		chipSpec: IOChipSpec,
		renderSpec: ChipRenderSpec,
	): IOChip {
		switch (chipSpec.ioChipType) {
			case "input":
				return new chipSpec.ChipClass(chipSpec, renderSpec);
			case "output":
				return new chipSpec.ChipClass(chipSpec, renderSpec);
		}
	}
}
