import type { Chip } from "../entities/chip";
import { CompositeChip } from "../entities/composite-chip";
import type { IEvents } from "../services/eventing-service";
import type { Simulator } from "../simulator";
import { didAnyChange } from "../utils";
import { BaseManager } from "./base-manager";

export class ChipManager extends BaseManager {
	public readonly chips: Chip[];

	constructor(sim: Simulator) {
		super(sim);

		this.chips = [];

		this.init();
	}

	public init(): void {
		this.sim.on("chip.spawn", (chipSpec) => this.onSpawnChip(chipSpec));
	}

	public executeChips(): boolean {
		return didAnyChange(this.chips, (chip) => chip.execute());
	}

	public getChipById(chipId: string): Chip | undefined {
		return this.chips.find((chip) => chip.id === chipId);
	}

	// TODO: better name
	public commitAllPinValues(): boolean {
		return didAnyChange(this.chips, (chip) => chip.commitPinValues());
	}

	private onSpawnChip(chipSpec: IEvents["chip.spawn"]): void {
		let chip: Chip;

		switch (chipSpec.type) {
			case "io":
				chip = new chipSpec.ChipClass(chipSpec);
				break;
			case "atomic":
				chip = new chipSpec.ChipClass(chipSpec);
				break;
			case "composite":
				chip = new CompositeChip(chipSpec);
				break;
		}

		this.chips.push(chip);
	}
}
