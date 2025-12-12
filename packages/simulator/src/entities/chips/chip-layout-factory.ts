import { renderEngineConfig } from "@digital-logic-sim/render-engine";
import type { PinType } from "../pin";
import type { Chip, ChipSpec } from "./chip.interface";
import { ChipUtils } from "./chip.utils";
import type { RectDimensions, Position } from "@digital-logic-sim/shared-types";

const chipLayoutConfig = {
	aspectRatio: 1.5,
};

export interface ChipLayout {
	dimensions: RectDimensions;
	getPinOffset(pinType: PinType): Position;
	getPinPosition(pinIdx: number, pinType: PinType): Position;
}

export class ChipLayoutFactory implements ChipLayout {
	public dimensions: RectDimensions;

	constructor(private readonly chip: Pick<Chip, "spec" | "renderState">) {
		this.dimensions = computeChipDimensions(chip.spec);
	}

	public getPinOffset(pinType: PinType) {
		const [numInputPins, numOutputPins] = [
			this.chip.spec.inputPins.length,
			this.chip.spec.outputPins.length,
		];

		const [height, width] = [this.dimensions.height, this.dimensions.width];

		const { x: chipX, y: chipY } = this.chip.renderState.position;

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
			this.chip.spec,
			this.chip.renderState.position,
			this.dimensions,
		);

		const pinOffset = pinType === "in" ? inputPinOffset : outputPinOffset;

		return {
			x: pinOffset.x,
			y: pinOffset.y - renderEngineConfig.pinSize * 3 * pinIdx,
		};
	}
}

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
