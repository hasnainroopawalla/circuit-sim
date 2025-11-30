type RenderableType = "chip" | "wire";

export type Position = {
	x: number;
	y: number;
};

export type Dimension = {
	width: number;
	height: number;
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

export type ChipRenderable = BaseRenderable<"chip"> & {
	label: string;
	position: Position;
	dimensions: Dimension;
};

export type WireRenderable = BaseRenderable<"wire"> & {
	controlPoints: Float32Array;
};

export type Renderable = ChipRenderable | WireRenderable;

export type CameraProjectionData = {
	viewProjectionMatrix: Float32Array;
	viewProjectionInvMatrix: Float32Array;
};
