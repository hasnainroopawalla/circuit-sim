import type { PinSpec, PinType } from "../../entities/pin";
import type { BlueprintSet, Blueprint } from "./blueprint-service.interface";

export const BlueprintUtils = {
	getRootBlueprint: (blueprint: BlueprintSet): Blueprint => {
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
	return Object.keys(
		pinType === "in" ? blueprint.inputMappings : blueprint.outputMappings,
	).map((pinName) => ({ name: pinName }));
}
