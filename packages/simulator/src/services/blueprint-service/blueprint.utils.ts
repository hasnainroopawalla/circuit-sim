import type { PinSpec, PinType } from "../../entities/pin";
import type { Blueprint, CompositeBlueprint } from "./blueprint-service";

export const BlueprintUtils = {
	getRootBlueprint: (blueprint: Blueprint): CompositeBlueprint => {
		return blueprint.definitions[blueprint.root];
	},

	getIOPinCount: (blueprint: Blueprint) => {
		return {
			numInputPins: getIOPinSpecs(blueprint, "in").length,
			numOutputPins: getIOPinSpecs(blueprint, "out").length,
		};
	},

	getIOPinSpecs: (
		blueprint: Blueprint,
	): {
		inputPins: PinSpec[];
		outputPins: PinSpec[];
	} => {
		return {
			inputPins: getIOPinSpecs(blueprint, "in"),
			outputPins: getIOPinSpecs(blueprint, "out"),
		};
	},
};

function getIOPinSpecs(blueprint: Blueprint, pinType: PinType): PinSpec[] {
	const rootBlueprint = BlueprintUtils.getRootBlueprint(blueprint);

	return Object.keys(
		pinType === "in"
			? rootBlueprint.inputMappings
			: rootBlueprint.outputMappings,
	).map((pinName) => ({ name: pinName }));
}
