import { entityIdService } from "../entities/services";
import { Entity } from "./entity";

export type PinType = "in" | "out";

export type PinSpec = {
	name: string;
	type: PinType;
};

export class Pin extends Entity {
	public readonly spec: PinSpec;

	constructor(spec: PinSpec) {
		super({
			id: entityIdService.getId(), // TODO, should not be only inputChipId
			type: "pin",
		});

		this.spec = spec;
	}
}
