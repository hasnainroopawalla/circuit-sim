import type { Pin } from "../pin";
import { BaseChip } from "./chip";
import type {
	ChipInitParams,
	EntitySpawnOptions,
	CompositeChipSpec,
	IOChipType,
	ChipType,
} from "./chip.interface";
import type { OutputChip, InputChip } from "./io-chip";

export type RuntimePinMapping<T extends IOChipType> = {
	internalChip: T extends IOChipType.Input ? InputChip : OutputChip;
	internalPin: Pin;
};

export class CompositeChip extends BaseChip<ChipType.Composite> {
	private inputMappings!: RuntimePinMapping<IOChipType.Input>[];
	private outputMappings!: RuntimePinMapping<IOChipType.Output>[];

	constructor(
		chipSpec: CompositeChipSpec,
		chipInitParams: ChipInitParams,
		opts?: EntitySpawnOptions,
	) {
		super(chipSpec, chipInitParams, opts);
	}

	public setMappings(
		inputMappings: RuntimePinMapping<IOChipType.Input>[],
		outputMappings: RuntimePinMapping<IOChipType.Output>[],
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
