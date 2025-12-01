import type { ColorRGBA } from "@digital-logic-sim/render-engine";
import { entityIdService } from "../entity-id-service";
import { BaseEntity } from "./entity";

export type WireSpec = {
	startPinId: string;
	endPinId: string;
};

export type WireRenderSpec = { color: ColorRGBA; controlPoints: Float32Array };

export class Wire extends BaseEntity<"wire"> {
	public readonly spec: WireSpec;
	public readonly renderSpec: WireRenderSpec;

	constructor(wireSpec: WireSpec, renderSpec: WireRenderSpec) {
		super({
			id: entityIdService.getId(), // TODO fix
			type: "wire",
		});

		this.spec = wireSpec;
		this.renderSpec = renderSpec;
	}
}
