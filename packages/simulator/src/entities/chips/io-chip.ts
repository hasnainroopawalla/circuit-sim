import { Chip } from "./chip";
import type { ChipRenderSpec, IOChipSpec } from "./chip.interface";

export abstract class IOChip extends Chip {
	constructor(chipSpec: IOChipSpec, renderSpec: ChipRenderSpec) {
		super(chipSpec, renderSpec);
	}
}

export class InputChip extends IOChip {
	private nextValue = false;

	constructor(chipSpec: IOChipSpec, renderSpec: ChipRenderSpec) {
		super(chipSpec, renderSpec);
	}

	public setValue(to: boolean): void {
		this.nextValue = to;
	}

	public execute(): boolean {
		const pin = this.outputPins[0];

		if (pin.nextValue !== this.nextValue) {
			pin.nextValue = this.nextValue;
			return true;
		}

		return false;
	}
}

export class OutputChip extends IOChip {
	constructor(chipSpec: IOChipSpec, renderSpec: ChipRenderSpec) {
		super(chipSpec, renderSpec);
	}

	public execute(): boolean {
		return false; // output chip does not perform any action
	}
}
