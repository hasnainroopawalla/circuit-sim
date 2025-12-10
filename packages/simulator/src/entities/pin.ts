import type { ColorRGBA, Position } from "@digital-logic-sim/render-engine";
import type { Chip } from "./chips";
import { BaseEntity } from "./entity";

export type PinType = "in" | "out";

export type PinSpec = {
	name: string;
};

export class Pin extends BaseEntity<"pin"> {
	public spec: PinSpec;

	public pinType: PinType;
	public pinIdx: number;

	public chip: Chip;

	public currentValue: boolean;
	public nextValue: boolean;

	constructor(args: {
		spec: PinSpec;
		id: string;
		chip: Chip;
		pinType: PinType;
		pinIdx: number; // TODO: required?
	}) {
		super({
			id: args.id,
			entityType: "pin",
		});

		this.spec = args.spec;

		this.chip = args.chip;

		this.pinType = args.pinType;
		this.pinIdx = args.pinIdx;

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

	public getPosition(): Position {
		// TODO: This can just be a util?
		return this.chip.layout.getPinPosition(
			this.pinIdx,
			this.pinType,
			this.chip.renderState.position,
		);
	}

	public getColor(): ColorRGBA {
		return {
			r: Number(!this.currentValue),
			g: Number(this.currentValue),
			b: 0.0,
			a: 1.0,
		};
	}
}
