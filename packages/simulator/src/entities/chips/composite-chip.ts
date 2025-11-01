import { Chip } from "./chip";
import type { ChipRenderSpec, CompositeChipSpec } from "./chip.interface";

export class CompositeChip extends Chip {
	constructor(chipSpec: CompositeChipSpec, renderSpec: ChipRenderSpec) {
		super(chipSpec, renderSpec);
	}

	public execute(): boolean {
		return false;
	}
}
