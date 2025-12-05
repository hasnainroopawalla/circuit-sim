import type { ColorRGBA, Position } from "@digital-logic-sim/render-engine";
import { entityIdService } from "../entity-id-service";
import { BaseEntity } from "./entity";
import type { Pin } from "./pin";

export type WireSpec = {
	startPinId: string;
	endPinId: string;
};

export type WireRenderSpec = { color: ColorRGBA; controlPoints: Position[] };

export class Wire extends BaseEntity<"wire"> {
	public spec: WireSpec;
	public renderSpec: WireRenderSpec;

	public startPin: Pin;
	public endPin: Pin;

	constructor(args: {
		spec: WireSpec;
		renderSpec: WireRenderSpec;
		startPin: Pin;
		endPin: Pin;
	}) {
		super({
			id: entityIdService.generateId(),
			entityType: "wire",
		});

		this.spec = args.spec;
		this.renderSpec = args.renderSpec;

		this.startPin = args.startPin;
		this.endPin = args.endPin;
	}

	public getPath(): WireRenderSpec["controlPoints"] {
		return [
			this.startPin.getPosition(),
			...this.renderSpec.controlPoints,
			this.endPin.getPosition(),
		];
	}
}
