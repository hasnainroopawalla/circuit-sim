import type { Position, ColorRGBA } from "@digital-logic-sim/shared-types";
import type { Chip } from "./chips";
import { BaseEntity } from "./entity";

export type PinType = "in" | "out";

export type PinSpec = {
	name: string;
};

type PinRenderState = {
	color: ColorRGBA;
	label?: string;
};

export class Pin extends BaseEntity<"pin"> {
	public spec: PinSpec;
	public renderState: PinRenderState;

	public pinType: PinType;
	public pinIdx: number;

	public chip: Chip;

	public currentValue: boolean;
	public nextValue: boolean;

	constructor(args: {
		spec: PinSpec;
		renderState: Omit<PinRenderState, "color">;
		chip: Chip;
		pinType: PinType;
		pinIdx: number;
	}) {
		super({
			entityType: "pin",
		});

		this.spec = args.spec;
		this.renderState = {
			...args.renderState,
			color: {
				r: 0,
				g: 0,
				b: 0,
				a: 1,
			},
		};

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
		return this.chip.layout.getPinPosition(this.pinIdx, this.pinType);
	}
}
