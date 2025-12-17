import type { ColorRGBA, Position } from "@digital-logic-sim/shared-types";
import { BaseEntity } from "./entity";
import type { Pin } from "./pin";
import type { EntitySpawnOptions } from "./chips";

export type WireConnection = { startPin: Pin; endPin: Pin };

export type WireInitParams = { color: ColorRGBA; controlPoints: Position[] };

export type WireRenderState = WireInitParams;

export class Wire extends BaseEntity<"wire"> {
	public renderState: WireRenderState;

	public startPin: Pin;
	public endPin: Pin;

	public parentCompositeId: string;

	constructor(args: {
		wireConnection: WireConnection;
		wireInitParams: WireInitParams;
		opts?: EntitySpawnOptions;
	}) {
		super({
			entityType: "wire",
		});

		this.startPin = args.wireConnection.startPin;
		this.endPin = args.wireConnection.endPin;

		this.parentCompositeId = args.opts?.parentCompositeId || "";

		this.renderState = {
			color: args.wireInitParams.color,
			controlPoints: args.wireInitParams.controlPoints,
		};
	}

	public getPath(): WireInitParams["controlPoints"] {
		return [
			this.startPin.getPosition(),
			...this.renderState.controlPoints,
			this.endPin.getPosition(),
		];
	}
}
