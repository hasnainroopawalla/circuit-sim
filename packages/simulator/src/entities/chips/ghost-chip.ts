import { type ChipLayout, ChipLayoutFactory } from "./chip-layout-factory";
import type {
	ChipInitParams,
	ChipRenderState,
	ChipSpec,
} from "./chip.interface";
import type { Position } from "@digital-logic-sim/shared-types";

export class GhostChip {
	public spec: ChipSpec;
	public layout: ChipLayout;
	public renderState: ChipRenderState;

	constructor(spec: ChipSpec, chipInitParams: ChipInitParams) {
		this.spec = spec;
		this.renderState = chipInitParams;

		this.layout = new ChipLayoutFactory(this.renderState, {
			numInputPins: this.spec.inputPins.length,
			numOutputPins: this.spec.outputPins.length,
		});
	}

	public setPosition(position: Position): void {
		this.renderState.position = position;
	}
}
