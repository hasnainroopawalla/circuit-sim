import type { ChipRenderState, ChipSpec } from "../../entities/chips";
import type { WireRenderState } from "../../entities/wire";

export type IOMapping = Record<
	string /* externalPinLabel */,
	BlueprintPinMapping[]
>;

export type Blueprint = {
	root: string;
	definitions: Record<string, CompositeDefinition>;
};

export type CompositeDefinition = {
	chips: ChipBlueprint[];
	wires: WireBlueprint[];

	inputMappings: IOMapping;
	outputMappings: IOMapping;
};

export type BlueprintPinMapping = {
	internalChipId: string;
	internalPinName: string;
};

export type ChipBlueprint = {
	id: string;
	spec: Pick<ChipSpec, "name" | "chipType">;
	renderState: ChipRenderState;
};

type WireConnectionBlueprint = {
	chipId: string;
	pinName: string;
};

export type WireBlueprint = {
	spec: {
		start: WireConnectionBlueprint;
		end: WireConnectionBlueprint;
	};
	renderState: WireRenderState;
};
