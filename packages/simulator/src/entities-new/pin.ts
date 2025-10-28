import { entityIdService } from "../entities/services";
import { Entity } from "./entity";

type PinDirection = "in" | "out";

export type PinSpec = {
	label: string;
	direction: PinDirection;
};

export class Pin extends Entity {
	public readonly spec: PinSpec;

	constructor(spec: PinSpec) {
		super({
			id: entityIdService.inputChipId(), // TODO, should not be only inputChipId
			type: "pin",
		});

		this.spec = spec;
	}
}
