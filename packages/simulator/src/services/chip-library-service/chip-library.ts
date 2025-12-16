import {
	AndChip,
	NotChip,
	OrChip,
	type AtomicChip,
	type AtomicChipSpec,
	type AtomicChipType,
	type InputChip,
	type InputChipSpec,
	type IOChipType,
	type OutputChip,
	type OutputChipSpec,
	type ChipSpec,
	type ChipSpawnOptions,
	type IOChipInitParams,
	type AtomicChipInitParams,
} from "../../entities/chips";

export type AtomicChipClass = new (
	chipSpec: AtomicChipSpec,
	chipInitParams: AtomicChipInitParams,
	opts?: ChipSpawnOptions,
) => AtomicChip;

export type IOChipClass<T extends IOChipType> = T extends "input"
	? new (
			spec: InputChipSpec,
			params: IOChipInitParams,
			opts?: ChipSpawnOptions,
		) => InputChip
	: new (
			spec: OutputChipSpec,
			params: IOChipInitParams,
			opts?: ChipSpawnOptions,
		) => OutputChip;

// TODO: dont export
export const IO_CHIP_SPECS: ChipSpec[] = [
	{
		name: "INPUT",
		chipType: "io",
		ioChipType: "input",
		inputPins: [],
		outputPins: [{ name: "in0" }],
	},
	{
		name: "OUTPUT",
		chipType: "io",
		ioChipType: "output",
		inputPins: [{ name: "out0" }],
		outputPins: [],
	},
];

const ATOMIC_CHIP_SPECS: ChipSpec[] = [
	{
		name: "AND",
		atomicChipType: "AND",
		chipType: "atomic",
		inputPins: [{ name: "in0" }, { name: "in1" }],
		outputPins: [{ name: "out0" }],
	},
	{
		name: "OR",
		atomicChipType: "OR",
		chipType: "atomic",
		inputPins: [{ name: "in0" }, { name: "in1" }],
		outputPins: [{ name: "out0" }],
	},
	{
		name: "NOT",
		atomicChipType: "NOT",
		chipType: "atomic",
		inputPins: [{ name: "in0" }],
		outputPins: [{ name: "out0" }],
	},
];

export const PRIMITIVE_CHIP_SPECS: ChipSpec[] = [
	...IO_CHIP_SPECS,
	...ATOMIC_CHIP_SPECS,
];

export const ATOMIC_CHIPS_MAP: Record<AtomicChipType, AtomicChipClass> = {
	AND: AndChip,
	OR: OrChip,
	NOT: NotChip,
};
