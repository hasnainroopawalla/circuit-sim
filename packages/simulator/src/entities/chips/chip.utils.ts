import {
	type Position,
	type RectDimensions,
	renderEngineConfig,
} from "@digital-logic-sim/render-engine";
import type { ChipSpec } from "./chip.interface";

export const ChipUtils = {
	getPinOffsets: (
		chipSpec: ChipSpec,
		position: Position,
		dimensions: RectDimensions,
	) => {
		const [numInputPins, numOutputPins] = [
			chipSpec.inputPins.length,
			chipSpec.outputPins.length,
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
