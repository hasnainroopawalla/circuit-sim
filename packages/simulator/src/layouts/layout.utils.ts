import type {
	ChipRenderable,
	PinRenderable,
} from "@digital-logic-sim/render-engine";
import type { Chip } from "../entities/chips";
import type { Pin } from "../entities/pin";
import { COLORS } from "../services/color-service";

export const LayoutUtils = {
	chipToRenderable: (chip: Chip, hoveredEntityId?: string): ChipRenderable => ({
		type: "chip",
		dimensions: chip.layout.dimensions,
		label: chip.spec.name,
		inputPins: getPinRenderables(chip.inputPins, hoveredEntityId),
		outputPins: getPinRenderables(chip.outputPins, hoveredEntityId),
		...chip.getRenderState(),
	}),
};

function getPinRenderables(
	pins: Pin[],
	hoveredEntityId?: string,
): PinRenderable[] {
	return pins.map((pin) => ({
		type: "pin",
		value: pin.currentValue,
		position: pin.getPosition(),
		color: hoveredEntityId === pin.id ? COLORS.White : pin.renderState.color,
	}));
}
