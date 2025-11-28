import type { ColorRGBA } from "@digital-logic-sim/render-engine";
import { entityIdService } from "../entity-id-service";
import { Entity } from "./entity";

export type WireSpec = {
	startPinId: string;
	endPinId: string;
};

type WireRenderSpec = { color: ColorRGBA };

export class Wire extends Entity {
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
