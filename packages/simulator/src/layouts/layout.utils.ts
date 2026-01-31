import {
	ChipRenderableType,
	RenderableType,
	type ChipRenderable,
	type PinRenderable,
} from "@digital-logic-sim/render-engine";
import type { Chip } from "../entities/chips";
import type { Pin } from "../entities/pin";
import { COLORS } from "../services/color-service";
import { EntityUtils } from "../entities/utils";

export const LayoutUtils = {
	chipToRenderable: (chip: Chip, hoveredEntityId?: string): ChipRenderable => {
		const chipRenderableType = EntityUtils.isIOChip(chip)
			? ChipRenderableType.Circle
			: ChipRenderableType.Rect;

		const chipRenderState = chip.getRenderState();

		return {
			type: RenderableType.Chip,
			chipRenderableType,
			dimensions: chip.layout.dimensions,
			inputPins: getPinRenderables(chip.inputPins, hoveredEntityId),
			outputPins: getPinRenderables(chip.outputPins, hoveredEntityId),
			color: chipRenderState.color,
			position: chipRenderState.position,
		};
	},
};

function getPinRenderables(
	pins: Pin[],
	hoveredEntityId?: string,
): PinRenderable[] {
	return pins.map((pin) => ({
		type: RenderableType.Pin,
		position: pin.getPosition(),
		color: hoveredEntityId === pin.id ? COLORS.White : pin.renderState.color,
	}));
}
