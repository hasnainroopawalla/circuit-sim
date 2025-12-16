import type { PinSpec } from "../pin";
import type { AtomicChip } from "./atomic-chip";
import type { InputChip, OutputChip } from "./io-chip";
import type { CompositeChip } from "./composite-chip";
import type { ColorRGBA, Position } from "@digital-logic-sim/shared-types";
import type { Blueprint } from "../../services/blueprint-service";

export type AtomicChipType = "AND" | "OR" | "NOT";

export type ChipType = "io" | "atomic" | "composite";
export type Chip = IOChip | AtomicChip | CompositeChip;

export type IOChipType = "input" | "output";
export type IOChip = InputChip | OutputChip;

export type ChipRenderState = ChipInitParams;

export type ChipSpawnOptions = {
	parentCompositeId?: string;
};

export type ChipInitParams = {
	color: ColorRGBA;
	position: Position;
};

type BaseChipSpec<TChipType extends ChipType> = {
	chipType: TChipType;
	name: string;
	inputPins: PinSpec[];
	outputPins: PinSpec[];
};

type BaseIOChipSpec<TIOChipType extends IOChipType> = BaseChipSpec<"io"> & {
	ioChipType: TIOChipType;
};

export type InputChipSpec = BaseIOChipSpec<"input">;
export type OutputChipSpec = BaseIOChipSpec<"output">;
export type IOChipSpec = InputChipSpec | OutputChipSpec;

export type AtomicChipSpec = BaseChipSpec<"atomic"> & {
	atomicChipType: AtomicChipType;
};

export type CompositeChipSpec = BaseChipSpec<"composite"> & {
	blueprint: Blueprint;
};

export type ChipSpec = IOChipSpec | AtomicChipSpec | CompositeChipSpec;

// export type IOChipInitParams = BaseChipInitParams<"io"> & {
// 	externalPinName: string;
// };
// export type AtomicChipInitParams = BaseChipInitParams<"atomic">;
// export type CompositeChipInitParams = BaseChipInitParams<"composite">;

// export type ChipInitParams =
// 	| AtomicChipInitParams
// 	| IOChipInitParams
// 	| CompositeChipInitParams;

export type IOChipInitParams = ChipInitParams & {
	externalPinName: string;
};
