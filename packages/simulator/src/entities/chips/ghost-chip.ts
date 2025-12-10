import type { Position } from "@digital-logic-sim/render-engine";
import { type ChipLayout, ChipLayoutFactory } from "./chip-layout-factory";
import type {
	ChipInitParams,
	ChipRenderState,
	ChipSpec,
} from "./chip.interface";

export class GhostChip {
	public layout: ChipLayout;
	public renderState: ChipRenderState;

	constructor(chipSpec: ChipSpec, chipInitParams: ChipInitParams) {
		this.layout = ChipLayoutFactory.create(chipSpec);
		this.renderState = chipInitParams;
	}

	public setPosition(position: Position): void {
		this.renderState.position = position;
	}
}
