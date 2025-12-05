import {
	renderEngineConfig,
	type Position,
} from "@digital-logic-sim/render-engine";
import type { Entity } from "./entities/entity";
import { ChipUtils, type Chip } from "./entities/chips";
import type { Pin } from "./entities/pin";

export const MeshUtils = {
	getHoveredChipEntity: (
		mouseWorldPosition: Position,
		chips: Chip[],
	): Entity | null => {
		for (let i = 0; i < chips.length; ++i) {
			const chip = chips[i];
			const [height, width] = [
				chip.renderState.dimensions.height,
				chip.renderState.dimensions.width,
			];

			const isMouseOverChip =
				mouseWorldPosition.x >
					chip.renderState.position.x - width - renderEngineConfig.pinSize &&
				mouseWorldPosition.x <
					chip.renderState.position.x + width + renderEngineConfig.pinSize &&
				mouseWorldPosition.y > chip.renderState.position.y - height &&
				mouseWorldPosition.y < chip.renderState.position.y + height;

			if (!isMouseOverChip) {
				continue;
			}

			const pin = MeshUtils.getPinUnderMouse(chip, mouseWorldPosition);

			if (pin) {
				return pin;
			}

			return chip;
		}
		return null;
	},
	getPinUnderMouse: (chip: Chip, mouseWorldPosition: Position): Pin | null => {
		const { inputPinOffset, outputPinOffset } = ChipUtils.getPinOffsets(chip);

		const boundingRadiusSq = 2 * renderEngineConfig.pinSize ** 2;

		const isInputSide = mouseWorldPosition.x > chip.renderState.position.x;

		const pins = isInputSide ? chip.inputPins : chip.outputPins;
		const offset = isInputSide ? inputPinOffset : outputPinOffset;

		for (let i = 0; i < pins.length; ++i) {
			const dx = mouseWorldPosition.x - offset.x;
			const dy =
				mouseWorldPosition.y - (offset.y - 3 * renderEngineConfig.pinSize * i);

			if (dx ** 2 + dy ** 2 <= boundingRadiusSq) {
				return pins[i];
			}
		}

		return null;
	},
};
