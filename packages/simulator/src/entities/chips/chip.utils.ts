import { renderEngineConfig } from "@digital-logic-sim/render-engine";
import type { Chip } from "./chip.interface";

export const ChipUtils = {
	getPinOffsets: (chip: Pick<Chip, "renderSpec" | "spec">) => {
		const [numInputPins, numOutputPins] = [
			chip.spec.inputPins.length,
			chip.spec.outputPins.length,
		];

		const [height, width] = [
			chip.renderSpec.dimensions.height,
			chip.renderSpec.dimensions.width,
		];

		const maxPins = Math.max(numInputPins, numOutputPins);

		return {
			inputPinOffset: {
				x: chip.renderSpec.position.x + width,
				y:
					chip.renderSpec.position.y +
					height -
					renderEngineConfig.pinSize * (2 + (3 * (maxPins - numInputPins)) / 2),
			},
			outputPinOffset: {
				x: chip.renderSpec.position.x - width,
				y:
					chip.renderSpec.position.y +
					height -
					renderEngineConfig.pinSize *
						(2 + (3 * (maxPins - numOutputPins)) / 2),
			},
		};
	},
};
