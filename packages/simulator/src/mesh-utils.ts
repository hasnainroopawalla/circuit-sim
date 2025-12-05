import {
	renderEngineConfig,
	type Position,
} from "@digital-logic-sim/render-engine";
import type { Entity } from "./entities/entity";
import type { Chip } from "./entities/chips";

// TODO: Fix this
export const MeshUtils = {
	getHoveredEntity: (
		mouseWorldPosition: Position,
		chips: Chip[],
	): Entity | null => {
		for (let i = 0; i < chips.length; ++i) {
			const chip = chips[i];
			const numInputPins = chip.inputPins.length;
			const numOutputPins = chip.outputPins.length;
			const maxPins = Math.max(numInputPins, numOutputPins);
			const height = (maxPins * 1.5 + 0.5) * renderEngineConfig.pinSize;
			// const width = renderEngineConfig.chipAspectRatio * height;
			const width = 1.5 * height;

			let interior = true;
			interior =
				mouseWorldPosition.x >
					chip.renderSpec.position.x - width - renderEngineConfig.pinSize &&
				interior;
			interior =
				mouseWorldPosition.x <
					chip.renderSpec.position.x + width + renderEngineConfig.pinSize &&
				interior;
			interior =
				mouseWorldPosition.y > chip.renderSpec.position.y - height && interior;
			interior =
				mouseWorldPosition.y < chip.renderSpec.position.y + height && interior;
			if (!interior) {
				continue;
			}
			const inputPinOffset = {
				x: chip.renderSpec.position.x + width,
				y:
					chip.renderSpec.position.y +
					height -
					renderEngineConfig.pinSize * (2 + (3 * (maxPins - numInputPins)) / 2),
			};
			const outputPinOffset = {
				x: chip.renderSpec.position.x - width,
				y:
					chip.renderSpec.position.y +
					height -
					renderEngineConfig.pinSize *
						(2 + (3 * (maxPins - chip.outputPins.length)) / 2),
			};
			const boundingRadiusSq = 2 * renderEngineConfig.pinSize ** 2;
			if (mouseWorldPosition.x > chip.renderSpec.position.x) {
				for (let j = 0; j < numInputPins; ++j) {
					const distx = (mouseWorldPosition.x - inputPinOffset.x) ** 2;
					const distSq =
						distx +
						(mouseWorldPosition.y -
							(inputPinOffset.y - 3 * renderEngineConfig.pinSize * j)) **
							2;
					if (distSq <= boundingRadiusSq) return chip.inputPins[j];
				}
			} else {
				const distx = (mouseWorldPosition.x - outputPinOffset.x) ** 2;
				for (let j = 0; j < numOutputPins; ++j) {
					const distSq =
						distx +
						(mouseWorldPosition.y -
							(outputPinOffset.y - 3 * renderEngineConfig.pinSize * j)) **
							2;
					if (distSq <= boundingRadiusSq) return chip.outputPins[j];
				}
			}
			return chip;
		}

		return null;
	},
	// getPinPosition(pin: Pin): Position {
	// 	const chip = pin.chip;

	// 	const numInputPins = chip.inputPins.length;
	// 	const numOutputPins = chip.outputPins.length;
	// 	const maxPins = Math.max(numInputPins, numOutputPins);
	// 	const height = (maxPins * 1.5 + 0.5) * renderEngineConfig.pinSize;
	// 	const width = renderEngineConfig.chipAspectRatio * height;
	// 	const inputPinOffset = {
	// 		x: chip.renderSpec.position.x + width,
	// 		y:
	// 			chip.renderSpec.position.y +
	// 			height -
	// 			renderEngineConfig.pinSize * (2 + (3 * (maxPins - numInputPins)) / 2),
	// 	};
	// 	const outputPinOffset = {
	// 		x: chip.renderSpec.position.x - width,
	// 		y:
	// 			chip.renderSpec.position.y +
	// 			height -
	// 			renderEngineConfig.pinSize *
	// 				(2 + (3 * (maxPins - chip.outputPins.length)) / 2),
	// 	};
	// 	if (pin.pinType === "in") {
	// 		const index = chip.inputPins.findIndex((element) => {
	// 			return element.id === pin.id;
	// 		});
	// 		return {
	// 			x: inputPinOffset.x,
	// 			y: inputPinOffset.y - 3 * renderEngineConfig.pinSize * index,
	// 		};
	// 	} else {
	// 		const index = chip.outputPins.findIndex((element) => {
	// 			return element.id === pin.id;
	// 		});
	// 		return {
	// 			x: outputPinOffset.x,
	// 			y: outputPinOffset.y - 3 * renderEngineConfig.pinSize * index,
	// 		};
	// 	}
	// },
};
