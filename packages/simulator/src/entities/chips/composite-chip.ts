import type { Position } from "@digital-logic-sim/render-engine";
import { BaseChip } from "./chip";
import type { ChipRenderSpec, CompositeChipSpec } from "./chip.interface";

export class CompositeChip extends BaseChip<"composite"> {
	constructor(
		chipSpec: CompositeChipSpec,
		renderSpec: ChipRenderSpec,
		initialPosition: Position,
	) {
		super(chipSpec, renderSpec, initialPosition);
	}

	public execute(): boolean {
		return false;
	}
}
