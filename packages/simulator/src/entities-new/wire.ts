import { entityIdService } from "../entities/services";
import { Entity } from "./entity";
import type { Pin } from "./pin";

export type WireSpec = {};

export class Wire extends Entity {
	public readonly spec: WireSpec;

	private readonly pins: Pin[];

	constructor(spec: WireSpec) {
		super({
			id: entityIdService.getId(), // TODO fix
			type: "wire",
		});

		this.spec = spec;
	}

	public connect(): void {}
}
