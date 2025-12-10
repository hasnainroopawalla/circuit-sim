import { BaseChip } from "./chip";
import type { AtomicChipSpec, ChipInitParams } from "./chip.interface";

export abstract class AtomicChip extends BaseChip<"atomic"> {
	constructor(chipSpec: AtomicChipSpec, chipInitParams: ChipInitParams) {
		super(chipSpec, chipInitParams);
	}
}

export class AndChip extends AtomicChip {
	constructor(chipSpec: AtomicChipSpec, chipInitParams: ChipInitParams) {
		super(chipSpec, chipInitParams);
	}

	public execute(): boolean {
		return super.setOutputPins([
			this.inputPins[0].currentValue && this.inputPins[1].currentValue,
		]);
	}
}

export class NotChip extends AtomicChip {
	constructor(chipSpec: AtomicChipSpec, chipInitParams: ChipInitParams) {
		super(chipSpec, chipInitParams);
	}

	public execute(): boolean {
		return super.setOutputPins([!this.inputPins[0].currentValue]);
	}
}
