import { Chip } from "./chip";
import type { AtomicChipSpec, ChipRenderSpec } from "./chip.interface";

export abstract class AtomicChip extends Chip {
	constructor(chipSpec: AtomicChipSpec, renderSpec: ChipRenderSpec) {
		super(chipSpec, renderSpec);
	}
}

export class AndChip extends AtomicChip {
	constructor(chipSpec: AtomicChipSpec, renderSpec: ChipRenderSpec) {
		super(chipSpec, renderSpec);
	}

	public execute(): boolean {
		return super.setOutputPins([
			this.inputPins[0].currentValue && this.inputPins[1].currentValue,
		]);
	}
}

export class NotChip extends AtomicChip {
	constructor(chipSpec: AtomicChipSpec, renderSpec: ChipRenderSpec) {
		super(chipSpec, renderSpec);
	}

	public execute(): boolean {
		return super.setOutputPins([!this.inputPins[0].currentValue]);
	}
}
