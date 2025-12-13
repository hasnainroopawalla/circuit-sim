import { BaseChip } from "./chip";
import type {
	AtomicChipSpec,
	ChipInitParams,
	ChipSpawnOptions,
} from "./chip.interface";

export abstract class AtomicChip extends BaseChip<"atomic"> {
	constructor(
		chipSpec: AtomicChipSpec,
		chipInitParams: ChipInitParams,
		opts?: ChipSpawnOptions,
	) {
		super(chipSpec, chipInitParams, opts);
	}
}

export class AndChip extends AtomicChip {
	constructor(
		chipSpec: AtomicChipSpec,
		chipInitParams: ChipInitParams,
		opts?: ChipSpawnOptions,
	) {
		super(chipSpec, chipInitParams, opts);
	}

	public execute(): boolean {
		return this.setOutputPins([
			this.inputPins[0].currentValue && this.inputPins[1].currentValue,
		]);
	}
}

export class OrChip extends AtomicChip {
	constructor(
		chipSpec: AtomicChipSpec,
		chipInitParams: ChipInitParams,
		opts?: ChipSpawnOptions,
	) {
		super(chipSpec, chipInitParams, opts);
	}

	public execute(): boolean {
		return this.setOutputPins([
			this.inputPins[0].currentValue || this.inputPins[1].currentValue,
		]);
	}
}

export class NotChip extends AtomicChip {
	constructor(
		chipSpec: AtomicChipSpec,
		chipInitParams: ChipInitParams,
		opts?: ChipSpawnOptions,
	) {
		super(chipSpec, chipInitParams, opts);
	}

	public execute(): boolean {
		return this.setOutputPins([!this.inputPins[0].currentValue]);
	}
}
