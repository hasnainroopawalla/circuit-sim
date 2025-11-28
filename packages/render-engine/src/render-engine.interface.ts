type RenderableType = "chip" | "wire";

type BaseRenderable<TRenderable extends RenderableType> = {
	type: TRenderable;
	color: {
		r:number;
		g:number;
		b:number;
		a:number;
	}
};

export type ChipRenderable = BaseRenderable<"chip"> & {
	label: string;
	position: {
		x: number;
		y: number;
	};
	dimensions: {
		width: number;
		height: number;
	}
};

export type WireRenderable = BaseRenderable<"wire">&{
	controlPoints: Float32Array;
}; // TODO: position data for wire?

export type Renderable = ChipRenderable | WireRenderable;

export type CameraEntity = {
	eye: Float32Array;
};
