import type {
	AtomicChip,
	IOChip,
	CompositeChip,
	AtomicChipSpec,
	IOChipSpec,
	CompositeChipSpec,
} from "../../entities/chips";
import type { AtomicChipFactory, IOChipFactory } from "./builtin-registry";
import type { ChipFactory } from "./chip-library-service";
import type { CompositeChipFactory } from "./composite-registry";

export type ChipFromFactory<T extends ChipFactory> = T extends AtomicChipFactory
	? AtomicChip
	: T extends IOChipFactory
		? IOChip
		: T extends CompositeChipFactory
			? CompositeChip
			: never;

export type ChipSpecFromFactory<T extends ChipFactory> =
	T extends AtomicChipFactory
		? AtomicChipSpec
		: T extends IOChipFactory
			? IOChipSpec
			: T extends CompositeChipFactory
				? CompositeChipSpec
				: never;

export const ChipLibraryUtils = {
	getChipSpec: <T extends ChipFactory>(
		chipFactory: T,
	): ChipSpecFromFactory<T> => {
		switch (chipFactory.kind) {
			case "atomic":
			case "io":
				return chipFactory.ChipClass.spec as ChipSpecFromFactory<T>;
			case "composite":
				return chipFactory.spec as ChipSpecFromFactory<T>;
		}
	},
};
