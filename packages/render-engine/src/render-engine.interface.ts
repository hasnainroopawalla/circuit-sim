type RenderableType = "chip" | "wire" | "pin";

export type Position = {
	x: number;
	y: number;
};

export type RectDimensions = {
	height: number;
	width: number;
};

export type ColorRGBA = {
	r: number;
	g: number;
	b: number;
	a: number;
};

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
	dimensions: RectDimensions;
	inputPins: PinRenderable[];
	outputPins: PinRenderable[];
};

export type WireRenderable = BaseRenderable<"wire"> & {
	controlPoints: Position[];
};

export type Renderable = ChipRenderable | WireRenderable;

export type CameraProjectionData = {
	viewProjectionMatrix: Float32Array;
	viewProjectionInvMatrix: Float32Array;
};
