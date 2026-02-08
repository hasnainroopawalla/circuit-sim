import { renderEngineConfig } from "@digital-logic-sim/render-engine";
import type { Position, RectDimension } from "@digital-logic-sim/shared-types";
import type { ChipType } from "./chip.interface";

export type ChipMetadata = {
	name: string;
	chipType: ChipType;
	numInputPins: number;
	numOutputPins: number;
};

export const ChipUtils = {
	getPinOffsets: (
		chipMetadata: ChipMetadata,
		position: Position,
		dimensions: RectDimension,
	) => {
		const [numInputPins, numOutputPins] = [
			chipMetadata.numInputPins,
			chipMetadata.numOutputPins,
		];

		const maxPins = Math.max(numInputPins, numOutputPins);

		return {
			inputPinOffset: {
				x: position.x + dimensions.width,
				y:
					position.y +
					dimensions.height -
					renderEngineConfig.pinSize * (2 + (3 * (maxPins - numInputPins)) / 2),
			},
			outputPinOffset: {
				x: position.x - dimensions.width,
				y:
					position.y +
					dimensions.height -
					renderEngineConfig.pinSize *
						(2 + (3 * (maxPins - numOutputPins)) / 2),
			},
		};
	},
	getIOPinOffsets: (position: Position, dimensions: RectDimension) => {
		return {
			inputPinOffset: {
				x: position.x + dimensions.width,
				y: position.y,
			},
			outputPinOffset: {
				x: position.x - dimensions.width,
				y: position.y,
			},
		};
	},
};

export const CHIP_LABEL_CONFIG = {
	maxCharsPerLine: 8,
	maxLines: 2,
	lineHeight: 0.1, // world units
	paddingX: 0.0,
	paddingY: 0.0,
	charWidth: 0.1,
};

export const ChipLabelUtils = {
	splitChipName(name: string): string[] {
		if (name.length <= CHIP_LABEL_CONFIG.maxCharsPerLine) {
			return [name];
		}

		const midpoint = Math.ceil(name.length / 2);
		return [name.slice(0, midpoint), name.slice(midpoint)];
	},
};
