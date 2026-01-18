import type {
	ColorRGBA,
	Position,
	RectDimension,
} from "@digital-logic-sim/shared-types";

type RenderableType = "chip" | "wire" | "pin";

type BaseRenderable<TRenderable extends RenderableType> = {
	type: TRenderable;
	color: ColorRGBA;
};

export type PinRenderable = BaseRenderable<"pin"> & {
	value: boolean;
	position: Position;
};

export type ChipRenderable = BaseRenderable<"chip"> & {
	label: string;
	position: Position;
	dimensions: RectDimension;
	inputPins: PinRenderable[];
	outputPins: PinRenderable[];
};

export type WireRenderable = BaseRenderable<"wire"> & {
	path: Position[];
};

export type Renderable = ChipRenderable | WireRenderable;

export type CameraProjectionData = {
	viewProjectionMatrix: Float32Array;
	viewProjectionInvMatrix: Float32Array;
};
