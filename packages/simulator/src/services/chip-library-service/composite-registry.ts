import type { CompositeChipSpec } from "../../entities/chips";
import type { ChipMetadata, ChipDefinition } from "./chip-library-service";

export type CompositeChipName = string;

export type CompositeChipFactory = {
	kind: "composite";
	spec: CompositeChipSpec;
	descriptor: ChipMetadata;
};

export class CompositeChipRegistry {
	private registry = new Map<CompositeChipName, CompositeChipSpec>();

	public register(spec: CompositeChipSpec): void {
		this.registry.set(spec.name, spec);
	}

	public getDefinitions(): ChipDefinition[] {
		return Array.from(this.registry.keys()).map((name) => ({
			kind: "composite",
			name,
		}));
	}

	public resolve(name: CompositeChipName): CompositeChipFactory {
		const spec = this.registry.get(name);
		if (!spec) {
			throw new Error(`Composite chip "${name}" not found`);
		}
		return {
			kind: "composite",
			spec,
			descriptor: {
				numInputPins: spec.inputPins.length,
				numOutputPins: spec.outputPins.length,
			},
		};
	}
}
