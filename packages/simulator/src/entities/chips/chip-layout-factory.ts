import {
	type Position,
	type RectDimensions,
	renderEngineConfig,
} from "@digital-logic-sim/render-engine";
import type { PinType } from "../pin";
import type { ChipSpec } from "./chip.interface";
import { ChipUtils } from "./chip.utils";

const chipLayoutConfig = {
	aspectRatio: 1.5,
};

export interface ChipLayout {
	dimensions: RectDimensions;
	getPinOffset(pinType: PinType, position: Position): Position;
	getPinPosition(
		pinIdx: number,
		pinType: PinType,
		position: Position,
	): Position;
}

export const ChipLayoutFactory = {
	create(spec: ChipSpec): ChipLayout {
		const dimensions = computeChipDimensions(spec);

		return {
			dimensions,
			getPinOffset(pinType: PinType, position: Position) {
				const [numInputPins, numOutputPins] = [
					spec.inputPins.length,
					spec.outputPins.length,
				];

				const [height, width] = [dimensions.height, dimensions.width];

				const maxPins = Math.max(numInputPins, numOutputPins);

				switch (pinType) {
					case "in":
						return {
							x: position.x + width,
							y:
								position.y +
								height -
								renderEngineConfig.pinSize *
									(2 + (3 * (maxPins - numInputPins)) / 2),
						};
					case "out":
						return {
							x: position.x - width,
							y:
								position.y +
								height -
								renderEngineConfig.pinSize *
									(2 + (3 * (maxPins - numOutputPins)) / 2),
						};
				}
			},

			getPinPosition(pinIdx: number, pinType: PinType, chipPosition: Position) {
				const { inputPinOffset, outputPinOffset } = ChipUtils.getPinOffsets(
					spec,
					chipPosition,
					dimensions,
				);

				const pinOffset = pinType === "in" ? inputPinOffset : outputPinOffset;

				return {
					x: pinOffset.x,
					y: pinOffset.y - renderEngineConfig.pinSize * 3 * pinIdx,
				};
			},
		};
	},
};

const computeChipDimensions = (chipSpec: ChipSpec): RectDimensions => {
	const [numInputPins, numOutputPins] = [
		chipSpec.inputPins.length,
		chipSpec.outputPins.length,
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
