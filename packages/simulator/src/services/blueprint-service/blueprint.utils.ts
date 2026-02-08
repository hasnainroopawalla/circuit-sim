import { type PinSpec, PinType } from "../../entities/pin";
import type { CompositeDefinition } from "./blueprint-service.interface";

export const BlueprintUtils = {
	getIOPinSpecs: (
		definition: CompositeDefinition,
	): {
		inputPins: PinSpec[];
		outputPins: PinSpec[];
	} => {
		return {
			inputPins: getIOPinSpecsOf(definition, PinType.In),
			outputPins: getIOPinSpecsOf(definition, PinType.Out),
		};
	},
};

function getIOPinSpecsOf(
	definition: CompositeDefinition,
	pinType: PinType,
): PinSpec[] {
	return Object.keys(
		pinType === PinType.In
			? definition.inputMappings
			: definition.outputMappings,
	).map((pinName) => ({ name: pinName }));
}
