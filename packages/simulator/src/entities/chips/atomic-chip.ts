import type { Position } from "@digital-logic-sim/render-engine";
import { BaseChip } from "./chip";
import type { AtomicChipSpec, ChipRenderSpec } from "./chip.interface";

export abstract class AtomicChip extends BaseChip<"atomic"> {
	constructor(
		chipSpec: AtomicChipSpec,
		renderSpec: ChipRenderSpec,
		initialPosition: Position,
	) {
		super(chipSpec, renderSpec, initialPosition);
	}
}

export class AndChip extends AtomicChip {
	constructor(
		chipSpec: AtomicChipSpec,
		renderSpec: ChipRenderSpec,
		initialPosition: Position,
	) {
		super(chipSpec, renderSpec, initialPosition);
	}

	public execute(): boolean {
		return super.setOutputPins([
			this.inputPins[0].currentValue && this.inputPins[1].currentValue,
		]);
	}
}

export class NotChip extends AtomicChip {
	constructor(
		chipSpec: AtomicChipSpec,
		renderSpec: ChipRenderSpec,
		initialPosition: Position,
	) {
		super(chipSpec, renderSpec, initialPosition);
	}

	public execute(): boolean {
		return super.setOutputPins([!this.inputPins[0].currentValue]);
	}
}
