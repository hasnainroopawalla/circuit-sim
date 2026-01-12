import type {
	ChipRenderable,
	PinRenderable,
} from "@digital-logic-sim/render-engine";
import type { Chip } from "../entities/chips";
import type { Pin } from "../entities/pin";

export const LayoutUtils = {
	chipToRenderable: (chip: Chip): ChipRenderable => ({
		type: "chip",
		dimensions: chip.layout.dimensions,
		label: chip.spec.name,
		inputPins: getPinRenderables(chip.inputPins),
		outputPins: getPinRenderables(chip.outputPins),
		...chip.getRenderState(),
	}),
};

function getPinRenderables(pins: Pin[]): PinRenderable[] {
	return pins.map((pin) => ({
		type: "pin",
		value: pin.currentValue,
		position: pin.getPosition(),
		color: pin.renderState.color,
	}));
}
