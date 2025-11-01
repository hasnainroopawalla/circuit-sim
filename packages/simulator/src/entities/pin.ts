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

	public commitValue(): boolean {
		if (this.currentValue !== this.nextValue) {
			this.currentValue = this.nextValue;
			console.log("Committing", this.spec.name, "to", this.nextValue);
			return true;
		}
		return false;
	}

	public execute(): void {}
}
