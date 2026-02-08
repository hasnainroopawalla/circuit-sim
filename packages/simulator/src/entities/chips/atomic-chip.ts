import { COLORS } from "../../services/color-service";
import { BaseChip } from "./chip";
import {
	AtomicChipType,
	ChipType,
	type AtomicChipSpec,
	type ChipInitParams,
	type EntitySpawnOptions,
} from "./chip.interface";

export abstract class AtomicChip extends BaseChip<ChipType.Atomic> {
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
		name: AtomicChipType.And,
		atomicChipType: AtomicChipType.And,
		chipType: ChipType.Atomic,
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
		name: AtomicChipType.Or,
		atomicChipType: AtomicChipType.Or,
		chipType: ChipType.Atomic,
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
		name: AtomicChipType.Not,
		atomicChipType: AtomicChipType.Not,
		chipType: ChipType.Atomic,
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
