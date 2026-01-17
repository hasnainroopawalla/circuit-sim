import { COLORS } from "../../services/color-service";
import { BaseChip } from "./chip";
import type {
	AtomicChipSpec,
	ChipInitParams,
	EntitySpawnOptions,
} from "./chip.interface";

export abstract class AtomicChip extends BaseChip<"atomic"> {
	constructor(
		chipSpec: AtomicChipSpec,
		chipInitParams: ChipInitParams,
		opts?: EntitySpawnOptions,
	) {
		super(chipSpec, chipInitParams, opts);
	}
}

export class AndChip extends AtomicChip {
	static readonly spec: AtomicChipSpec = {
		name: "AND",
		atomicChipType: "AND",
		chipType: "atomic",
		inputPins: [{ name: "in0" }, { name: "in1" }],
		outputPins: [{ name: "out0" }],
		color: COLORS.AndGate,
	};

	constructor(chipInitParams: ChipInitParams, opts?: EntitySpawnOptions) {
		super(AndChip.spec, chipInitParams, opts);
	}

	public execute(): boolean {
		return this.setOutputPins([
			this.inputPins[0].currentValue && this.inputPins[1].currentValue,
		]);
	}
}

export class OrChip extends AtomicChip {
	static readonly spec: AtomicChipSpec = {
		name: "OR",
		atomicChipType: "OR",
		chipType: "atomic",
		inputPins: [{ name: "in0" }, { name: "in1" }],
		outputPins: [{ name: "out0" }],
		color: COLORS.OrGate,
	};

	constructor(chipInitParams: ChipInitParams, opts?: EntitySpawnOptions) {
		super(OrChip.spec, chipInitParams, opts);
	}

	public execute(): boolean {
		return this.setOutputPins([
			this.inputPins[0].currentValue || this.inputPins[1].currentValue,
		]);
	}
}

export class NotChip extends AtomicChip {
	static readonly spec: AtomicChipSpec = {
		name: "NOT",
		atomicChipType: "NOT",
		chipType: "atomic",
		inputPins: [{ name: "in0" }],
		outputPins: [{ name: "out0" }],
		color: COLORS.NotGate,
	};

	constructor(chipInitParams: ChipInitParams, opts?: EntitySpawnOptions) {
		super(NotChip.spec, chipInitParams, opts);
	}

	public execute(): boolean {
		return this.setOutputPins([!this.inputPins[0].currentValue]);
	}
}
