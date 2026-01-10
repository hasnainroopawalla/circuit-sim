import type { PinSpec, PinType } from "../../entities/pin";
import type { CompositeDefinition } from "./blueprint-service.interface";

export const BlueprintUtils = {
	getIOPinSpecs: (
		definition: CompositeDefinition,
	): {
		inputPins: PinSpec[];
		outputPins: PinSpec[];
	} => {
		return {
			inputPins: getIOPinSpecsOf(definition, "in"),
			outputPins: getIOPinSpecsOf(definition, "out"),
		};
	},
};

function getIOPinSpecsOf(
	definition: CompositeDefinition,
	pinType: PinType,
): PinSpec[] {
	return Object.keys(
		pinType === "in" ? definition.inputMappings : definition.outputMappings,
	).map((pinName) => ({ name: pinName }));
}
