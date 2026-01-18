import { renderEngineConfig } from "@digital-logic-sim/render-engine";
import type { Position, RectDimension } from "@digital-logic-sim/shared-types";

export type ChipMetadata = {
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
};
