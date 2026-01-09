import type { CompositeChipSpec } from "../../entities/chips";
import { type Blueprint, BlueprintUtils } from "../blueprint-service";
import type { ChipDefinition } from "./chip-library-service";

export type CompositeChipName = string;

export type CompositeChipFactory = {
	kind: "composite";
	spec: CompositeChipSpec;
};

export class CompositeChipRegistry {
	private registry = new Map<CompositeChipName, CompositeChipSpec>();

	public register(blueprint: Blueprint): void {
		const { inputPins, outputPins } = BlueprintUtils.getIOPinSpecs(
			BlueprintUtils.getRootDefinition(blueprint),
		);

		this.registry.set(blueprint.root, {
			chipType: "composite",
			name: blueprint.root,
			blueprint,
			inputPins,
			outputPins,
		});
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
		};
	}
}
