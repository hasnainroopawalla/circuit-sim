import type { Pin } from "../pin";
import { BaseChip } from "./chip";
import type {
	ChipInitParams,
	EntitySpawnOptions,
	CompositeChipSpec,
	IOChipType,
} from "./chip.interface";
import type { OutputChip, InputChip } from "./io-chip";

export type RuntimePinMapping<T extends IOChipType> = {
	internalChip: T extends "input" ? InputChip : OutputChip;
	internalPin: Pin;
};

export class CompositeChip extends BaseChip<"composite"> {
	private inputMappings!: RuntimePinMapping<"input">[];
	private outputMappings!: RuntimePinMapping<"output">[];

	constructor(
		chipSpec: CompositeChipSpec,
		chipInitParams: ChipInitParams,
		opts?: EntitySpawnOptions,
	) {
		super(chipSpec, chipInitParams, opts);
	}

	public setMappings(
		inputMappings: RuntimePinMapping<"input">[],
		outputMappings: RuntimePinMapping<"output">[],
	): void {
		this.inputMappings = inputMappings;
		this.outputMappings = outputMappings;
	}

	public execute(): boolean {
		this.inputMappings.forEach((mapping, idx) => {
			mapping.internalChip.setValue(this.inputPins[idx].currentValue);
		});

		const outputs = this.outputMappings.map(
			(mapping) => mapping.internalPin.currentValue,
		);

		return this.setOutputPins(outputs);
	}
}
