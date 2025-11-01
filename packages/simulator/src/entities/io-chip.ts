import { Chip, type IOChipSpec } from "./chip";

export abstract class IOChip extends Chip {
	constructor(chipSpec: IOChipSpec) {
		super(chipSpec);
	}
}

export class InputChip extends IOChip {
	private nextValue = false;

	constructor(chipSpec: IOChipSpec) {
		super(chipSpec);
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
	constructor(chipSpec: IOChipSpec) {
		super(chipSpec);
	}

	public execute(): boolean {
		return false; // output chip does not perform any action
	}
}
