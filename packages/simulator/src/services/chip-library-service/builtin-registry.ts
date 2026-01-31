import {
	AndChip,
	OrChip,
	NotChip,
	InputChip,
	OutputChip,
	type IOChip,
	type AtomicChip,
	type EntitySpawnOptions,
	type IOChipType,
	type AtomicChipType,
	type ChipInitParams,
	type IOChipSpec,
	type AtomicChipSpec,
} from "../../entities/chips";
import { ChipNotFoundError } from "../../errors/chip-not-found-error";
import type { ChipDefinition } from "./chip-library-service";

type AtomicChipClass = (new (
	chipInitParams: ChipInitParams,
	opts?: EntitySpawnOptions,
) => AtomicChip) & { spec: AtomicChipSpec };

type IOChipClass = (new (
	chipInitParams: ChipInitParams,
	opts?: EntitySpawnOptions,
) => IOChip) & { spec: IOChipSpec };

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
};

export type IOChipFactory = {
	kind: "io";
	ChipClass: IOChipClass;
};

export class BuiltinChipRegistry {
	public getDefinitions(): ChipDefinition[] {
		const ioChipDefinitions = Object.values(BUILTIN_IO_CHIPS).map((chip) => ({
			kind: "io" as const,
			name: chip.spec.name as IOChipType,
		}));

		const atomicChipDefinitions = Object.values(BUILTIN_ATOMIC_CHIPS).map(
			(chip) => ({
				kind: "atomic" as const,
				name: chip.spec.name as AtomicChipType,
			}),
		);

		return [...ioChipDefinitions, ...atomicChipDefinitions];
	}

	public get(
		kind: "atomic" | "io",
		name: AtomicChipType | IOChipType,
	): AtomicChipFactory | IOChipFactory {
		switch (kind) {
			case "atomic": {
				const atomicChipClass = BUILTIN_ATOMIC_CHIPS[name as AtomicChipType];

				if (!atomicChipClass) {
					throw new ChipNotFoundError("atomic", name);
				}

				return {
					kind: "atomic",
					ChipClass: atomicChipClass,
				};
			}
			case "io": {
				const ioChipClass = BUILTIN_IO_CHIPS[name as IOChipType];

				if (!ioChipClass) {
					throw new ChipNotFoundError("io", name);
				}

				return {
					kind: "io",
					ChipClass: ioChipClass,
				};
			}
		}
	}
}
