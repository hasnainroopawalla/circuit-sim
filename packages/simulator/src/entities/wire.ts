import type { ColorRGBA, Position } from "@digital-logic-sim/shared-types";
import { BaseEntity } from "./entity";
import type { Pin } from "./pin";
import type { EntitySpawnOptions } from "./chips";
import { COLORS } from "../services/color-service";

export type WireConnection = { startPin: Pin; endPin: Pin };

export type WireInitParams = { controlPoints: Position[] };

export type WireRenderState = WireInitParams & { color: ColorRGBA };

export class Wire extends BaseEntity<"wire"> {
	public startPin: Pin;
	public endPin: Pin;

	public parentCompositeId: string;

	private renderState: WireRenderState;

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
			color: COLORS.LowSignal,
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

	public getRenderState(): WireRenderState {
		return {
			color: this.startPin.currentValue ? COLORS.HighSignal : COLORS.LowSignal,
			controlPoints: this.renderState.controlPoints,
		};
	}
}
