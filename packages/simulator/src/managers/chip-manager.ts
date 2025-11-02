import { type Chip, CompositeChip } from "../entities/chips";
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

	protected init(): void {
		this.sim.on("chip.spawn", this.onSpawnChip.bind(this));
	}

	private onSpawnChip(chipSpec: IEvents["chip.spawn"]): void {
		// TODO: should renderSpec be defined here?
		const renderSpec = {
			position: {
				x: 100,
				y: 200,
			},
			color: "#123456",
		};

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
