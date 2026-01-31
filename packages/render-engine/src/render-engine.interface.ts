import type {
	ColorRGBA,
	Position,
	RectDimension,
} from "@digital-logic-sim/shared-types";

export enum ChipRenderableType {
	Rect = "Rect",
	Circle = "Circle",
}

export enum RenderableType {
	Chip = "Chip",
	Wire = "Wire",
	Pin = "Pin",
}

type BaseRenderable<TRenderable extends RenderableType> = {
	type: TRenderable;
	color: ColorRGBA;
};

export type PinRenderable = BaseRenderable<RenderableType.Pin> & {
	position: Position;
};

export type ChipRenderable = BaseRenderable<RenderableType.Chip> & {
	chipRenderableType: ChipRenderableType;
	position: Position;
	dimensions: RectDimension;
	inputPins: PinRenderable[];
	outputPins: PinRenderable[];
};

export type WireRenderable = BaseRenderable<RenderableType.Wire> & {
	path: Position[];
};

export type Renderable = ChipRenderable | WireRenderable;

export type CameraProjectionData = {
	viewProjectionMatrix: Float32Array;
	viewProjectionInvMatrix: Float32Array;
};
