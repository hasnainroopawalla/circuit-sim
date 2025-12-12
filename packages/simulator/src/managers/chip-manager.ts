import {
	type Chip,
	type ChipSpec,
	type ChipInitParams,
	type IOChipSpec,
	type IOChip,
	type AtomicChipSpec,
	type AtomicChip,
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
				chip = this.createAtomicChip(chipSpec, chipInitParams);
				break;
			case "composite":
				chip = new CompositeChip(chipSpec, chipInitParams);
				break;
		}

		this.chips.push(chip);
		this.sim.emit("entity.spawn.finish", {
			entity: { id: chip.id, name: chip.spec.name },
		});
	}

	private createIOChip(
		chipSpec: IOChipSpec,
		chipInitParams: ChipInitParams,
	): IOChip {
		switch (chipSpec.ioChipType) {
			case "input": {
				const InputChipClass = this.sim.chipLibraryService.getInputChipClass();
				return new InputChipClass(chipSpec, chipInitParams);
			}
			case "output": {
				const OutputChipClass =
					this.sim.chipLibraryService.getOutputChipClass();
				return new OutputChipClass(chipSpec, chipInitParams);
			}
		}
	}

	private createAtomicChip(
		chipSpec: AtomicChipSpec,
		chipInitParams: ChipInitParams,
	): AtomicChip {
		const AtomicChipClass = this.sim.chipLibraryService.getAtomicChipClass(
			chipSpec.atomicChipType,
		);
		return new AtomicChipClass(chipSpec, chipInitParams);
	}
}
