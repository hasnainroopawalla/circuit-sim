import {
	AndChip,
	AtomicChipType,
	IOChipType,
	OrChip,
	NotChip,
	InputChip,
	OutputChip,
	type IOChip,
	type AtomicChip,
	type EntitySpawnOptions,
	type ChipInitParams,
	type IOChipSpec,
	type AtomicChipSpec,
	ChipType,
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
	[AtomicChipType.And]: AndChip,
	[AtomicChipType.Or]: OrChip,
	[AtomicChipType.Not]: NotChip,
};

const BUILTIN_IO_CHIPS: Record<IOChipType, IOChipClass> = {
	[IOChipType.Input]: InputChip,
	[IOChipType.Output]: OutputChip,
};

export type AtomicChipFactory = {
	kind: ChipType.Atomic;
	ChipClass: AtomicChipClass;
};

export type IOChipFactory = {
	kind: ChipType.IO;
	ChipClass: IOChipClass;
};

export class BuiltinChipRegistry {
	public getDefinitions(): ChipDefinition[] {
		const ioChipDefinitions = Object.values(BUILTIN_IO_CHIPS).map((chip) => ({
			kind: ChipType.IO as const,
			name: chip.spec.name as IOChipType,
		}));

		const atomicChipDefinitions = Object.values(BUILTIN_ATOMIC_CHIPS).map(
			(chip) => ({
				kind: ChipType.Atomic as const,
				name: chip.spec.name as AtomicChipType,
			}),
		);

		return [...ioChipDefinitions, ...atomicChipDefinitions];
	}

	public get(
		kind: ChipType.Atomic | ChipType.IO,
		name: AtomicChipType | IOChipType,
	): AtomicChipFactory | IOChipFactory {
		switch (kind) {
			case ChipType.Atomic: {
				const atomicChipClass = BUILTIN_ATOMIC_CHIPS[name as AtomicChipType];

				if (!atomicChipClass) {
					throw new ChipNotFoundError(ChipType.Atomic, name);
				}

				return {
					kind: ChipType.Atomic,
					ChipClass: atomicChipClass,
				};
			}
			case ChipType.IO: {
				const ioChipClass = BUILTIN_IO_CHIPS[name as IOChipType];

				if (!ioChipClass) {
					throw new ChipNotFoundError(ChipType.IO, name);
				}

				return {
					kind: ChipType.IO,
					ChipClass: ioChipClass,
				};
			}
		}
	}
}
