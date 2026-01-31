import { renderEngineConfig } from "@digital-logic-sim/render-engine";
import type { PinType } from "../pin";
import type { Chip } from "./chip.interface";
import {
	CHIP_LABEL_CONFIG,
	ChipLabelUtils,
	type ChipMetadata,
	ChipUtils,
} from "./chip.utils";
import type { RectDimension, Position } from "@digital-logic-sim/shared-types";

const chipLayoutConfig = {
	aspectRatio: 1.5,
	ioChipDimensions: {
		height: 0.25,
		width: 0.25,
	},
};

export interface ChipLayout {
	dimensions: RectDimension;
	getPinOffset(pinType: PinType): Position;
	getPinPosition(pinIdx: number, pinType: PinType): Position;
}

export class ChipLayoutFactory implements ChipLayout {
	public dimensions: RectDimension;

	private chipRenderState: Chip["renderState"];
	private chipMetadata: ChipMetadata;

	constructor(
		chipRenderState: Chip["renderState"],
		chipMetadata: ChipMetadata,
	) {
		this.chipRenderState = chipRenderState;
		this.chipMetadata = chipMetadata;

		switch (chipMetadata.chipType) {
			case "io":
				this.dimensions = chipLayoutConfig.ioChipDimensions;
				break;
			case "atomic":
			case "composite":
				this.dimensions = computeChipDimensions(chipMetadata);
				break;
		}
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
		const { inputPinOffset, outputPinOffset } =
			this.chipMetadata.chipType === "io"
				? ChipUtils.getIOPinOffsets(
						this.chipRenderState.position,
						this.dimensions,
					)
				: ChipUtils.getPinOffsets(
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

function computeChipDimensions(chipMetadata: ChipMetadata): RectDimension {
	const maxPins = Math.max(
		chipMetadata.numInputPins,
		chipMetadata.numOutputPins,
	);

	const lines = ChipLabelUtils.splitChipName(chipMetadata.name);
	const numLines = Math.min(lines.length, CHIP_LABEL_CONFIG.maxLines);

	const height =
		(maxPins * chipLayoutConfig.aspectRatio + 0.5) *
			renderEngineConfig.pinSize +
		numLines * CHIP_LABEL_CONFIG.lineHeight +
		CHIP_LABEL_CONFIG.paddingY;

	const longestLine = Math.max(...lines.map((l) => l.length));

	const width = Math.max(
		CHIP_LABEL_CONFIG.minWidth,
		longestLine * 0.12 + CHIP_LABEL_CONFIG.paddingX,
	);

	return { width, height };
}
