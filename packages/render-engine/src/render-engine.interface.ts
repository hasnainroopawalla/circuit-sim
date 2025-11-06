type BaseRenderViewEntity = {
	color: string;
};

type RenderViewChip = BaseRenderViewEntity & {
	position: {
		x: number;
		y: number;
	};
};

type RenderViewWire = BaseRenderViewEntity; // TODO: position data for wire?

export type RenderView = {
	entities: {
		chips: RenderViewChip[]; // TODO: how to represent pins?
		wires: RenderViewWire[];
	};
};

export type Renderable = {
	// TODO: define this interface
};
