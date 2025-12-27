import type { Pin } from "../pin";
import { BaseChip } from "./chip";
import type {
	EntitySpawnOptions,
	ChipInitParams,
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

	constructor(
		ioChipSpec: IOChipSpecOf<TIOChipType>,
		chipInitParams: ChipInitParams,
		opts?: EntitySpawnOptions,
	) {
		super(ioChipSpec, chipInitParams, opts);

		this.ioChipType = ioChipSpec.ioChipType;
	}

	public getPin(): Pin {
		return this.ioChipType === "input" ? this.outputPins[0] : this.inputPins[0];
	}
}

export class InputChip extends BaseIOChip<"input"> {
	static readonly spec: InputChipSpec = {
		name: "input",
		chipType: "io" as const,
		ioChipType: "input" as const,
		inputPins: [],
		outputPins: [{ name: "in" }],
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

export class OutputChip extends BaseIOChip<"output"> {
	static readonly spec: OutputChipSpec = {
		name: "output",
		chipType: "io" as const,
		ioChipType: "output" as const,
		inputPins: [{ name: "out" }],
		outputPins: [],
	};

	constructor(chipInitParams: ChipInitParams, opts?: EntitySpawnOptions) {
		super(OutputChip.spec, chipInitParams, opts);
	}

	public execute(): boolean {
		return false; // output chip does not perform any action
	}
}
