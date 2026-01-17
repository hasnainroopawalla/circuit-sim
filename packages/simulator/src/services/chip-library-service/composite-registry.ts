import type { CompositeChipSpec } from "../../entities/chips";
import { ChipNotFoundError } from "../../errors/chip-not-found-error";
import { type Blueprint, BlueprintUtils } from "../blueprint-service";
import { ColorService } from "../color-service";
import type { ChipDefinition } from "./chip-library-service";

export type CompositeChipName = string;

export type CompositeChipFactory = {
	kind: "composite";
	spec: CompositeChipSpec;
};

export class CompositeChipRegistry {
	private registry: Map<CompositeChipName, CompositeChipSpec>;

	constructor() {
		this.registry = new Map();
	}

	public register(blueprint: Blueprint): void {
		Object.entries(blueprint.definitions).forEach(([name, definition]) => {
			const { inputPins, outputPins } =
				BlueprintUtils.getIOPinSpecs(definition);

			this.registry.set(name, {
				chipType: "composite",
				name,
				definition,
				inputPins,
				outputPins,
				color: ColorService.generateChipColor(),
			});
		});
	}

	public getDefinitions(): ChipDefinition[] {
		return Array.from(this.registry.keys()).map((name) => ({
			kind: "composite",
			name,
		}));
	}

	public get(name: CompositeChipName): CompositeChipFactory {
		const compositeChipSpec = this.registry.get(name);

		if (!compositeChipSpec) {
			throw new ChipNotFoundError("composite", name);
		}

		return {
			kind: "composite",
			spec: compositeChipSpec,
		};
	}
}
