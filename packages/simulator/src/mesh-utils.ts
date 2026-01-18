import { renderEngineConfig } from "@digital-logic-sim/render-engine";
import type { Entity } from "./entities/entity";
import { ChipUtils, type Chip } from "./entities/chips";
import type { Pin } from "./entities/pin";
import type { Position, RectDimension } from "@digital-logic-sim/shared-types";

export const MeshUtils = {
	getHoveredChipEntity: (
		mouseWorldPosition: Position,
		chips: Chip[],
	): Entity | null => {
		for (let i = 0; i < chips.length; ++i) {
			const chip = chips[i];
			const [height, width] = [
				chip.layout.dimensions.height,
				chip.layout.dimensions.width,
			];

			const { position: chipPosition } = chip.getRenderState();

			const isMouseOverChip =
				mouseWorldPosition.x >
					chipPosition.x - width - renderEngineConfig.pinSize &&
				mouseWorldPosition.x <
					chipPosition.x + width + renderEngineConfig.pinSize &&
				mouseWorldPosition.y > chipPosition.y - height &&
				mouseWorldPosition.y < chipPosition.y + height;

			if (!isMouseOverChip) {
				continue;
			}

			const pin = getPinUnderMouse(chip, mouseWorldPosition);

			if (pin) {
				return pin;
			}

			return chip;
		}
		return null;
	},

	getWorldSpaceBoundingBox: (
		entityPosition: Position,
		entityDimension: RectDimension,
	): {
		minPosition: Position;
		maxPosition: Position;
	} => {
		return {
			minPosition: {
				x: entityPosition.x - entityDimension.width,
				y: entityPosition.y - entityDimension.height,
			},
			maxPosition: {
				x: entityPosition.x + entityDimension.width,
				y: entityPosition.y + entityDimension.height,
			},
		};
	},
};

function getPinUnderMouse(
	chip: Chip,
	mouseWorldPosition: Position,
): Pin | null {
	const { position: chipPosition } = chip.getRenderState();
	const { inputPinOffset, outputPinOffset } = ChipUtils.getPinOffsets(
		{
			numInputPins: chip.spec.inputPins.length,
			numOutputPins: chip.spec.outputPins.length,
		},
		chipPosition,
		chip.layout.dimensions,
	);

	const boundingRadiusSq = 2 * renderEngineConfig.pinSize ** 2;

	const isInputSide = mouseWorldPosition.x > chipPosition.x;

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
}
