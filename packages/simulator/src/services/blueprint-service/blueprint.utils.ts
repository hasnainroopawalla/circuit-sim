import type { PinSpec, PinType } from "../../entities/pin";
import type {
	Blueprint,
	CompositeDefinition,
} from "./blueprint-service.interface";

export const BlueprintUtils = {
	getRootDefinition: (blueprint: Blueprint): CompositeDefinition => {
		return blueprint.definitions[blueprint.root];
	},

	getCompositeChipDefinition: (
		chipName: string,
		blueprint: Blueprint,
	): CompositeDefinition => {
		// TODO: should return CompositeDefinition | undefined
		return blueprint.definitions[chipName];
	},

	getRootIOPinCount: (blueprint: Blueprint) => {
		const definition = BlueprintUtils.getRootDefinition(blueprint);
		return {
			numInputPins: getIOPinSpecs(definition, "in").length,
			numOutputPins: getIOPinSpecs(definition, "out").length,
		};
	},

	getIOPinSpecs: (
		definition: CompositeDefinition,
	): {
		inputPins: PinSpec[];
		outputPins: PinSpec[];
	} => {
		return {
			inputPins: getIOPinSpecs(definition, "in"),
			outputPins: getIOPinSpecs(definition, "out"),
		};
	},
};

function getIOPinSpecs(
	definition: CompositeDefinition,
	pinType: PinType,
): PinSpec[] {
	return Object.keys(
		pinType === "in" ? definition.inputMappings : definition.outputMappings,
	).map((pinName) => ({ name: pinName }));
}
