import { renderEngineConfig } from "@digital-logic-sim/render-engine";
import type { PinType } from "../pin";
import type { Chip } from "./chip.interface";
import { type ChipMetadata, ChipUtils } from "./chip.utils";
import type { RectDimension, Position } from "@digital-logic-sim/shared-types";

const chipLayoutConfig = {
	aspectRatio: 1.5,
};

export interface ChipLayout {
	dimensions: RectDimension;
	getPinOffset(pinType: PinType): Position;
	getPinPosition(pinIdx: number, pinType: PinType): Position;
}

export class ChipLayoutFactory implements ChipLayout {
	public dimensions: RectDimension;

	constructor(
		private readonly chipRenderState: Chip["renderState"],
		private readonly chipMetadata: ChipMetadata,
	) {
		this.dimensions = computeChipDimensions(chipMetadata);
	}

	public getPinOffset(pinType: PinType) {
		const [numInputPins, numOutputPins] = [
			this.chipMetadata.numInputPins,
			this.chipMetadata.numOutputPins,
		];

		const [height, width] = [this.dimensions.height, this.dimensions.width];

		const { x: chipX, y: chipY } = this.chipRenderState.position;

		const maxPins = Math.max(numInputPins, numOutputPins);

		switch (pinType) {
			case "in":
				return {
					x: chipX + width,
					y:
						chipY +
						height -
						renderEngineConfig.pinSize *
							(2 + (3 * (maxPins - numInputPins)) / 2),
				};
			case "out":
				return {
					x: chipX - width,
					y:
						chipY +
						height -
						renderEngineConfig.pinSize *
							(2 + (3 * (maxPins - numOutputPins)) / 2),
				};
		}
	}

	public getPinPosition(pinIdx: number, pinType: PinType): Position {
		const { inputPinOffset, outputPinOffset } = ChipUtils.getPinOffsets(
			this.chipMetadata,
			this.chipRenderState.position,
			this.dimensions,
		);

		const pinOffset = pinType === "in" ? inputPinOffset : outputPinOffset;

		return {
			x: pinOffset.x,
			y: pinOffset.y - renderEngineConfig.pinSize * 3 * pinIdx,
		};
	}
}

const computeChipDimensions = (chipMetadata: ChipMetadata): RectDimension => {
	const [numInputPins, numOutputPins] = [
		chipMetadata.numInputPins,
		chipMetadata.numOutputPins,
	];

	const maxPins = Math.max(numInputPins, numOutputPins);

	const height =
		(maxPins * chipLayoutConfig.aspectRatio + 0.5) * renderEngineConfig.pinSize;
	const width = chipLayoutConfig.aspectRatio * height;

	return {
		height,
		width,
	};
};
