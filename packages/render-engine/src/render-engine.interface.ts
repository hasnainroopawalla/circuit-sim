type RenderableType = "chip" | "wire";

type BaseRenderable<TRenderable extends RenderableType> = {
	type: TRenderable;
	color: string;
};

export type ChipRenderable = BaseRenderable<"chip"> & {
	position: {
		x: number;
		y: number;
	};
};

type WireRenderable = BaseRenderable<"wire">; // TODO: position data for wire?

export type Renderable = ChipRenderable | WireRenderable;
