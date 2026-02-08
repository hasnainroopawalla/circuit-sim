import type { ColorRGBA } from "@digital-logic-sim/shared-types";
import type { Pin } from "../pin";
import { BaseChip } from "./chip";
import {
	type EntitySpawnOptions,
	type ChipInitParams,
	type IOChipSpec,
	type InputChipSpec,
	type OutputChipSpec,
	IOChipType,
	ChipType,
} from "./chip.interface";
import { COLORS } from "../../services/color-service";

type IOChipSpecOf<TIOChipType> = Extract<
	IOChipSpec,
	{ ioChipType: TIOChipType }
>;

export abstract class BaseIOChip<
	TIOChipType extends IOChipType,
> extends BaseChip<ChipType.IO> {
	public ioChipType: TIOChipType;

	constructor(
		ioChipSpec: IOChipSpecOf<TIOChipType>,
		chipInitParams: ChipInitParams,
		opts?: EntitySpawnOptions,
	) {
		super(ioChipSpec, chipInitParams, opts);

		this.ioChipType = ioChipSpec.ioChipType;
	}

	public getPin(): Pin {
		return this.ioChipType === IOChipType.Input
			? this.outputPins[0]
			: this.inputPins[0];
	}

	override getColor(): ColorRGBA {
		return this.getPin().currentValue ? COLORS.HighSignal : COLORS.LowSignal;
	}
}

export class InputChip extends BaseIOChip<IOChipType.Input> {
	static readonly spec: InputChipSpec = {
		name: IOChipType.Input,
		chipType: ChipType.IO,
		ioChipType: IOChipType.Input,
		inputPins: [],
		outputPins: [{ name: "in" }],
		color: COLORS.Ghost /* unused */,
	};

	private nextValue = false;

	constructor(chipInitParams: ChipInitParams, opts?: EntitySpawnOptions) {
		super(InputChip.spec, chipInitParams, opts);
	}

	public toggle(): void {
		this.nextValue = !this.getPin().currentValue;
	}

	public setValue(value: boolean): void {
		this.nextValue = value;
	}

	public execute(): boolean {
		const pin = this.getPin();

		if (pin.nextValue !== this.nextValue) {
			pin.nextValue = this.nextValue;
			return true;
		}

		return false;
	}
}

export class OutputChip extends BaseIOChip<IOChipType.Output> {
	static readonly spec: OutputChipSpec = {
		name: IOChipType.Output,
		chipType: ChipType.IO,
		ioChipType: IOChipType.Output,
		inputPins: [{ name: "out" }],
		outputPins: [],
		color: COLORS.Ghost /* unused */,
	};

	constructor(chipInitParams: ChipInitParams, opts?: EntitySpawnOptions) {
		super(OutputChip.spec, chipInitParams, opts);
	}

	public execute(): boolean {
		return false; // output chip does not perform any action
	}
}
