import { renderEngineConfig } from "@digital-logic-sim/render-engine";
import type { Chip } from "./chip.interface";

export const ChipUtils = {
	getPinOffsets: (chip: Pick<Chip, "renderSpec" | "spec" | "renderState">) => {
		const [numInputPins, numOutputPins] = [
			chip.spec.inputPins.length,
			chip.spec.outputPins.length,
		];

		const [height, width] = [
			chip.renderState.dimensions.height,
			chip.renderState.dimensions.width,
		];

		const maxPins = Math.max(numInputPins, numOutputPins);

		return {
			inputPinOffset: {
				x: chip.renderState.position.x + width,
				y:
					chip.renderState.position.y +
					height -
					renderEngineConfig.pinSize * (2 + (3 * (maxPins - numInputPins)) / 2),
			},
			outputPinOffset: {
				x: chip.renderState.position.x - width,
				y:
					chip.renderState.position.y +
					height -
					renderEngineConfig.pinSize *
						(2 + (3 * (maxPins - numOutputPins)) / 2),
			},
		};
	},
};
