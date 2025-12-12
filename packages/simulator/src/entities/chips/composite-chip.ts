import { BaseChip } from "./chip";
import type { ChipInitParams, CompositeChipSpec } from "./chip.interface";

export class CompositeChip extends BaseChip<"composite"> {
	constructor(chipSpec: CompositeChipSpec, chipInitParams: ChipInitParams) {
		super(chipSpec, chipInitParams);
	}

	public execute(): boolean {
		console.log("execute", this);
		return false;
	}
}
