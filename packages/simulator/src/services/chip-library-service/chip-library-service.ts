import {
	type AtomicChipType,
	type IOChipType,
	ChipType,
} from "../../entities/chips";
import type { Blueprint } from "../blueprint-service";
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
	[ChipType.Atomic]: {
		name: AtomicChipType;
		resolved: AtomicChipFactory;
	};

	[ChipType.IO]: {
		name: IOChipType;
		resolved: IOChipFactory;
	};

	[ChipType.Composite]: {
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

	public register(blueprint: Blueprint): void {
		this.composite.register(blueprint);
	}

	public getAllDefinitions(): ChipDefinition[] {
		return [
			...this.builtin.getDefinitions(),
			...this.composite.getDefinitions(),
		];
	}

	public getChipFactory<TDef extends ChipDefinition>(
		definition: TDef,
	): ChipRegistryMap[TDef["kind"]]["resolved"] {
		switch (definition.kind) {
			case ChipType.Atomic:
			case ChipType.IO:
				return this.builtin.get(definition.kind, definition.name);
			case ChipType.Composite:
				return this.composite.get(definition.name);
		}
	}
}
