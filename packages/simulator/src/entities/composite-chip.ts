import { Chip, type CompositeChipSpec } from "./chip";

export class CompositeChip extends Chip {
	constructor(chipSpec: CompositeChipSpec) {
		super(chipSpec);
	}

	public execute(): boolean {
		return false;
	}
}
