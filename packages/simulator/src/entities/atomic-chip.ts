import { Chip, type AtomicChipSpec } from "./chip";

export abstract class AtomicChip extends Chip {
	constructor(chipSpec: AtomicChipSpec) {
		super(chipSpec);
	}
}

export class AndChip extends AtomicChip {
	constructor(chipSpec: AtomicChipSpec) {
		super(chipSpec);
	}

	public execute(): boolean {
		return super.setOutputPins([
			this.inputPins[0].currentValue && this.inputPins[1].currentValue,
		]);
	}
}

export class NotChip extends AtomicChip {
	constructor(chipSpec: AtomicChipSpec) {
		super(chipSpec);
	}

	public execute(): boolean {
		return super.setOutputPins([!this.inputPins[0].currentValue]);
	}
}
