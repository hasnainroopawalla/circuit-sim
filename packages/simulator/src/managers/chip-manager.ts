import {
	type Chip,
	type ChipSpec,
	type ChipInitParams,
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

	public spawnChip(chipSpec: ChipSpec, chipInitParams: ChipInitParams): void {
		let chip: Chip;

		switch (chipSpec.chipType) {
			case "io":
				chip = this.createIOChip(chipSpec, chipInitParams);
				break;
			case "atomic":
				chip = new chipSpec.ChipClass(chipSpec, chipInitParams);
				break;
			case "composite":
				chip = new CompositeChip(chipSpec, chipInitParams);
				break;
		}

		this.chips.push(chip);
		this.sim.emit("entity.spawned", {
			entityId: chip.id,
			entityName: chip.spec.name,
		});
	}

	private createIOChip(
		chipSpec: IOChipSpec,
		chipInitParams: ChipInitParams,
	): IOChip {
		switch (chipSpec.ioChipType) {
			case "input":
				return new chipSpec.ChipClass(chipSpec, chipInitParams);
			case "output":
				return new chipSpec.ChipClass(chipSpec, chipInitParams);
		}
	}
}
