import {
	AndChip,
	OrChip,
	NotChip,
	InputChip,
	OutputChip,
	type IOChip,
	type AtomicChip,
	type ChipSpawnOptions,
	type IOChipType,
	type AtomicChipType,
	type IOChipInitParams,
	type ChipInitParams,
} from "../../entities/chips";
import type { ChipMetadata, ChipDefinition } from "./chip-library-service";

// TODO: update to represent `spec` is a static class attr.
type AtomicChipClass = new (
	chipInitParams: ChipInitParams,
	opts?: ChipSpawnOptions,
) => AtomicChip;

type IOChipClass = new (
	chipInitParams: IOChipInitParams,
	opts?: ChipSpawnOptions,
) => IOChip;

const BUILTIN_ATOMIC_CHIPS: Record<AtomicChipType, AtomicChipClass> = {
	AND: AndChip,
	OR: OrChip,
	NOT: NotChip,
};

const BUILTIN_IO_CHIPS: Record<IOChipType, IOChipClass> = {
	input: InputChip,
	output: OutputChip,
};

export type AtomicChipFactory = {
	kind: "atomic";
	ChipClass: AtomicChipClass;
	descriptor: ChipMetadata;
};

export type IOChipFactory = {
	kind: "io";
	ioChipType: IOChipType;
	ChipClass: IOChipClass;
	descriptor: ChipMetadata;
};

export class BuiltinChipRegistry {
	public getDefinitions(): ChipDefinition[] {
		const ioChipDefinitions = Object.entries(BUILTIN_IO_CHIPS).map(
			([chipName, chip]) => ({
				kind: "io" as const,
				name: chipName as IOChipType,
			}),
		);

		const atomicChipDefinitions = Object.entries(BUILTIN_ATOMIC_CHIPS).map(
			([chipName, chip]) => ({
				kind: "atomic" as const,
				name: chipName as AtomicChipType,
			}),
		);

		return [...ioChipDefinitions, ...atomicChipDefinitions];
	}

	public resolve(
		kind: "atomic" | "io",
		name: AtomicChipType | IOChipType,
	): AtomicChipFactory | IOChipFactory {
		switch (kind) {
			case "atomic": {
				const atomicChipClass = BUILTIN_ATOMIC_CHIPS[name as AtomicChipType];

				return {
					kind: "atomic",
					ChipClass: atomicChipClass,
					descriptor: {
						numInputPins: atomicChipClass.spec.inputPins.length,
						numOutputPins: atomicChipClass.spec.outputPins.length,
					},
				};
			}
			case "io": {
				const ioChipClass = BUILTIN_IO_CHIPS[name as IOChipType];

				return {
					kind: "io",
					ioChipType: "input", // TODO: dont hardcode
					ChipClass: ioChipClass,
					descriptor: {
						numInputPins: ioChipClass.spec.inputPins.length,
						numOutputPins: ioChipClass.spec.outputPins.length,
					},
				};
			}
		}
	}
}
