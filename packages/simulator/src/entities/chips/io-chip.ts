import type { Pin } from "../pin";
import { BaseChip } from "./chip";
import type {
	IOChipInitParams,
	ChipSpawnOptions,
	InputChipSpec,
	IOChipSpec,
	IOChipType,
	OutputChipSpec,
} from "./chip.interface";

type IOChipSpecOf<TIOChipType> = Extract<
	IOChipSpec,
	{ ioChipType: TIOChipType }
>;

export abstract class BaseIOChip<
	TIOChipType extends IOChipType,
> extends BaseChip<"io"> {
	public ioChipType: TIOChipType;
	public externalPinName: string;

	constructor(
		ioChipSpec: IOChipSpecOf<TIOChipType>,
		chipInitParams: IOChipInitParams,
		opts?: ChipSpawnOptions,
	) {
		super(ioChipSpec, chipInitParams, opts);

		this.ioChipType = ioChipSpec.ioChipType;
		this.externalPinName = chipInitParams.externalPinName;
	}

	public getPin(): Pin {
		return this.ioChipType === "input" ? this.outputPins[0] : this.inputPins[0];
	}
}

export class InputChip extends BaseIOChip<"input"> {
	private nextValue = false;

	constructor(
		chipSpec: InputChipSpec,
		chipInitParams: IOChipInitParams,
		opts?: ChipSpawnOptions,
	) {
		super(chipSpec, chipInitParams, opts);
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

export class OutputChip extends BaseIOChip<"output"> {
	constructor(
		chipSpec: OutputChipSpec,
		chipInitParams: IOChipInitParams,
		opts?: ChipSpawnOptions,
	) {
		super(chipSpec, chipInitParams, opts);
	}

	public execute(): boolean {
		return false; // output chip does not perform any action
	}
}
