import type { ChipRenderable } from "@digital-logic-sim/render-engine";
import type { Chip } from "../entities/chips";

export const LayoutUtils = {
	chipToRenderable: (chip: Chip): ChipRenderable => ({
		type: "chip",
		color: chip.renderState.color,
		position: chip.renderState.position,
		dimensions: chip.layout.dimensions,
		label: chip.spec.name,
		inputPins: chip.inputPins.map((pin) => ({
			type: "pin",
			value: pin.currentValue,
			position: pin.getPosition(),
			color: pin.getColor(),
		})),
		outputPins: chip.outputPins.map((pin) => ({
			type: "pin",
			value: pin.currentValue,
			position: pin.getPosition(),
			color: pin.getColor(),
		})),
	}),
};
