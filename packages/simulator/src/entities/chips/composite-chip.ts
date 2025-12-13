import { BaseChip } from "./chip";
import type {
	ChipInitParams,
	ChipSpawnOptions,
	CompositeChipSpec,
} from "./chip.interface";

export class CompositeChip extends BaseChip<"composite"> {
	constructor(
		chipSpec: CompositeChipSpec,
		chipInitParams: ChipInitParams,
		opts?: ChipSpawnOptions,
	) {
		super(chipSpec, chipInitParams, opts);
	}

	public execute(): boolean {
		return false;
	}
}
