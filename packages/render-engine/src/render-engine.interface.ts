type RenderableType = "chip" | "wire";

type BaseRenderable<TRenderable extends RenderableType> = {
	type: TRenderable;
	color: string;
};

export type ChipRenderable = BaseRenderable<"chip"> & {
	label: string;
	position: {
		x: number;
		y: number;
	};
};

type WireRenderable = BaseRenderable<"wire">; // TODO: position data for wire?

export type Renderable = ChipRenderable | WireRenderable;

export type CameraEntity = {
	position: {
		x: number;
		y: number;
		z: number;
	};
	fov: number;
};
