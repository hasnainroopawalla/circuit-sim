import { Entity } from "./entity";

export type PinType = "in" | "out";

export type PinSpec = {
	name: string;
	type: PinType;
};

export class Pin extends Entity {
	public readonly spec: PinSpec;

	public currentValue: boolean;
	public nextValue: boolean;

	constructor(spec: PinSpec, id: string) {
		super({
			id,
			type: "pin",
		});

		this.spec = spec;

		this.currentValue = false;
		this.nextValue = false;
	}

	public commitValue(): void {
		this.currentValue = this.nextValue;
	}

	public execute(): void {}
}
