import type {
	AtomicChipType,
	CompositeChipSpec,
	IOChipType,
} from "../../entities/chips";
import {
	type AtomicChipFactory,
	type IOChipFactory,
	BuiltinChipRegistry,
} from "./builtin-registry";
import {
	type CompositeChipFactory,
	type CompositeChipName,
	CompositeChipRegistry,
} from "./composite-registry";

type ChipRegistryMap = {
	atomic: {
		definition: { name: AtomicChipType };
		resolved: AtomicChipFactory;
	};

	io: {
		definition: { name: IOChipType };
		resolved: IOChipFactory;
	};

	composite: {
		definition: { name: CompositeChipName };
		resolved: CompositeChipFactory;
	};
};

// TODO: is the best way to represent a skeleton?
export type ChipMetadata = {
	numInputPins: number;
	numOutputPins: number;
};

export type ChipDefinition = {
	[K in keyof ChipRegistryMap]: {
		kind: K;
	} & ChipRegistryMap[K]["definition"];
}[keyof ChipRegistryMap];

export type ChipFactory =
	| AtomicChipFactory
	| IOChipFactory
	| CompositeChipFactory;

export class ChipLibraryService {
	private builtin = new BuiltinChipRegistry();
	private composite = new CompositeChipRegistry();

	constructor() {}

	public register(spec: CompositeChipSpec): void {
		this.composite.register(spec);
	}

	public getAllDefinitions(): ChipDefinition[] {
		return [
			...this.builtin.getDefinitions(),
			...this.composite.getDefinitions(),
		];
	}

	public resolve<T extends ChipDefinition>(
		chipRef: T,
	): ChipRegistryMap[T["kind"]]["resolved"] {
		switch (chipRef.kind) {
			case "atomic":
			case "io":
				return this.builtin.resolve(chipRef.kind, chipRef.name);
			case "composite":
				return this.composite.resolve(chipRef.name);
		}
	}
}
