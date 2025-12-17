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
		name: AtomicChipType;
		resolved: AtomicChipFactory;
	};

	io: {
		name: IOChipType;
		resolved: IOChipFactory;
	};

	composite: {
		name: CompositeChipName;
		resolved: CompositeChipFactory;
	};
};

export type ChipDefinition = {
	[K in keyof ChipRegistryMap]: {
		kind: K;
		name: ChipRegistryMap[K]["name"];
	};
}[keyof ChipRegistryMap];

export type ChipFactory =
	| AtomicChipFactory
	| IOChipFactory
	| CompositeChipFactory;

export class ChipLibraryService {
	private builtin: BuiltinChipRegistry;
	private composite: CompositeChipRegistry;

	constructor() {
		this.builtin = new BuiltinChipRegistry();
		this.composite = new CompositeChipRegistry();
	}

	public register(spec: CompositeChipSpec): void {
		this.composite.register(spec);
	}

	public getAllDefinitions(): ChipDefinition[] {
		return [
			...this.builtin.getDefinitions(),
			...this.composite.getDefinitions(),
		];
	}

	public resolve<TDef extends ChipDefinition>(
		definition: TDef,
	): ChipRegistryMap[TDef["kind"]]["resolved"] {
		switch (definition.kind) {
			case "atomic":
			case "io":
				return this.builtin.resolve(definition.kind, definition.name);
			case "composite":
				return this.composite.resolve(definition.name);
		}
	}
}
