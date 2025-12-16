import { type ChipLayout, ChipLayoutFactory } from "./chip-layout-factory";
import type { ChipInitParams, ChipRenderState } from "./chip.interface";
import type { ChipMetadata } from "../../services/chip-library-service";
import type { Position } from "@digital-logic-sim/shared-types";

export class GhostChip {
	public descriptor: ChipMetadata;
	public layout: ChipLayout;
	public renderState: ChipRenderState;

	constructor(descriptor: ChipMetadata, chipInitParams: ChipInitParams) {
		this.descriptor = descriptor;
		this.renderState = chipInitParams;

		this.layout = new ChipLayoutFactory(this.renderState, this.descriptor);
	}

	public setPosition(position: Position): void {
		this.renderState.position = position;
	}
}
