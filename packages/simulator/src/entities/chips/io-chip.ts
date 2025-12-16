import type { Pin } from "../pin";
import { BaseChip } from "./chip";
import type {
	ChipSpawnOptions,
	IOChipInitParams,
	IOChipSpec,
	IOChipType,
	InputChipSpec,
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
	static readonly spec: InputChipSpec = {
		name: "INPUT",
		chipType: "io" as const,
		ioChipType: "input" as const,
		inputPins: [],
		outputPins: [{ name: "in0" }],
	};

	private nextValue = false;

	constructor(chipInitParams: IOChipInitParams, opts?: ChipSpawnOptions) {
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

export class OutputChip extends BaseIOChip<"output"> {
	static readonly spec: OutputChipSpec = {
		name: "OUTPUT",
		chipType: "io" as const,
		ioChipType: "output" as const,
		inputPins: [{ name: "out0" }],
		outputPins: [],
	};

	constructor(chipInitParams: IOChipInitParams, opts?: ChipSpawnOptions) {
		super(OutputChip.spec, chipInitParams, opts);
	}

	public execute(): boolean {
		return false; // output chip does not perform any action
	}
}
