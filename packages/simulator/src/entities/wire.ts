import type { ColorRGBA, Position } from "@digital-logic-sim/shared-types";
import { BaseEntity } from "./entity";
import type { Pin } from "./pin";

export type WireSpec = {
	startPinId: string;
	endPinId: string;
};

export type WireInitParams = { color: ColorRGBA; controlPoints: Position[] };

export type WireRenderState = WireInitParams;

export class Wire extends BaseEntity<"wire"> {
	public spec: WireSpec;

	public renderState: WireRenderState;

	public startPin: Pin;
	public endPin: Pin;

	constructor(args: {
		spec: WireSpec;
		wireInitParams: WireInitParams;
		startPin: Pin;
		endPin: Pin;
	}) {
		super({
			entityType: "wire",
		});

		this.spec = args.spec;

		this.renderState = {
			color: args.wireInitParams.color,
			controlPoints: args.wireInitParams.controlPoints,
		};

		this.startPin = args.startPin;
		this.endPin = args.endPin;
	}

	public getPath(): WireInitParams["controlPoints"] {
		return [
			this.startPin.getPosition(),
			...this.renderState.controlPoints,
			this.endPin.getPosition(),
		];
	}
}
