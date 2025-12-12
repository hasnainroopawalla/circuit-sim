import {
	AndChip,
	NotChip,
	OrChip,
	type AtomicChip,
	type AtomicChipSpec,
	type AtomicChipType,
	type ChipInitParams,
	type InputChip,
	type InputChipSpec,
	type IOChipType,
	type OutputChip,
	type OutputChipSpec,
	type ChipSpec,
} from "../../entities/chips";

export type AtomicChipClass = new (
	chipSpec: AtomicChipSpec,
	chipInitParams: ChipInitParams,
) => AtomicChip;

export type IOChipClass<T extends IOChipType> = T extends "input"
	? new (
			spec: InputChipSpec,
			params: ChipInitParams,
		) => InputChip
	: new (
			spec: OutputChipSpec,
			params: ChipInitParams,
		) => OutputChip;

const IO_CHIPS: ChipSpec[] = [
	{
		name: "INPUT",
		chipType: "io",
		ioChipType: "input",
		inputPins: [],
		outputPins: [{ name: "out 0" }],
	},
	{
		name: "OUTPUT",
		chipType: "io",
		ioChipType: "output",
		inputPins: [{ name: "in 0" }],
		outputPins: [],
	},
];

const ATOMIC_CHIPS: ChipSpec[] = [
	{
		name: "AND",
		atomicChipType: "AND",
		chipType: "atomic",
		inputPins: [{ name: "and in 0" }, { name: "and in 1" }],
		outputPins: [{ name: "and out 0" }],
	},
	{
		name: "OR",
		atomicChipType: "OR",
		chipType: "atomic",
		inputPins: [{ name: "or in 0" }, { name: "or in 1" }],
		outputPins: [{ name: "or out 0" }],
	},
	{
		name: "NOT",
		atomicChipType: "NOT",
		chipType: "atomic",
		inputPins: [{ name: "not in 0" }],
		outputPins: [{ name: "not out 0" }],
	},
];

export const PRIMITIVE_CHIP_SPECS: ChipSpec[] = [...IO_CHIPS, ...ATOMIC_CHIPS];

export const ATOMIC_CHIPS_MAP: Record<AtomicChipType, AtomicChipClass> = {
	AND: AndChip,
	OR: OrChip,
	NOT: NotChip,
};
