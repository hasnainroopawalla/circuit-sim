import { Chip, type CompositeChipSpec } from "./chip";

export class CompositeChip extends Chip {
	constructor(chipSpec: CompositeChipSpec) {
		super(chipSpec);
	}

	public execute(inputs: boolean[]): boolean[] {
		return inputs;
	}
}
