import type { Chip } from "./chips";
import { BaseEntity } from "./entity";

export type PinType = "in" | "out";

export type PinSpec = {
	name: string;
	pinType: PinType;
};

export class Pin extends BaseEntity<"pin"> {
	public spec: PinSpec;

	public chip: Chip;

	public currentValue: boolean;
	public nextValue: boolean;

	constructor(args: { spec: PinSpec; id: string; chip: Chip }) {
		super({
			id: args.id,
			type: "pin",
		});

		this.spec = args.spec;
		this.chip = args.chip;

		this.currentValue = false;
		this.nextValue = false;
	}

	public commitValue(): boolean {
		if (this.currentValue !== this.nextValue) {
			this.currentValue = this.nextValue;
			return true;
		}
		return false;
	}

	public execute(): void {}
}
