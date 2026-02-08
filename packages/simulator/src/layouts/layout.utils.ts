import {
	ChipRenderableType,
	RenderableType,
	type ChipRenderable,
	type PinRenderable,
} from "@digital-logic-sim/render-engine";
import { type Chip, ChipType } from "../entities/chips";
import type { Pin } from "../entities/pin";
import { COLORS } from "../services/color-service";

export const LayoutUtils = {
	chipToRenderable: (chip: Chip, hoveredEntityId?: string): ChipRenderable => {
		const chipRenderState = chip.getRenderState();

		return {
			type: RenderableType.Chip,
			chipRenderableType: LayoutUtils.getChipRenderableType(chip.chipType),
			dimensions: chip.layout.dimensions,
			inputPins: getPinRenderables(chip.inputPins, hoveredEntityId),
			outputPins: getPinRenderables(chip.outputPins, hoveredEntityId),
			color: chipRenderState.color,
			position: chipRenderState.position,
		};
	},

	getChipRenderableType: (chipType: ChipType): ChipRenderableType => {
		return chipType === ChipType.IO
			? ChipRenderableType.Circle
			: ChipRenderableType.Rect;
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
