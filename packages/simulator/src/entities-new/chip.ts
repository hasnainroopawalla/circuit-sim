import { entityIdService } from "../entities/services";
import { Entity } from "./entity";
import { Pin, type PinSpec } from "./pin";

export type ChipSpec = {
	label: string;
	pins: PinSpec[];
	execute: (inputs: boolean[]) => boolean[];
};

export class Chip extends Entity {
	public readonly spec: ChipSpec;

	private readonly pins: Pin[];

	constructor(spec: ChipSpec) {
		super({ id: entityIdService.chipId(spec.label), type: "chip" });

		this.spec = spec;
		this.pins = spec.pins.map((pinSpec) => new Pin(pinSpec));
	}
}
