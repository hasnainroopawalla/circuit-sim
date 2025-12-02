import { BaseChip } from "./chip";
import type { ChipRenderSpec, CompositeChipSpec } from "./chip.interface";

export class CompositeChip extends BaseChip<"composite"> {
	constructor(chipSpec: CompositeChipSpec, renderSpec: ChipRenderSpec) {
		super(chipSpec, renderSpec);
	}

	public execute(): boolean {
		return false;
	}
}
