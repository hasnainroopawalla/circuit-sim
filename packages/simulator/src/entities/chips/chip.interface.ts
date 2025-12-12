import type { PinSpec } from "../pin";
import type { AtomicChip } from "./atomic-chip";
import type { InputChip, OutputChip } from "./io-chip";
import type { CompositeChip } from "./composite-chip";
import type { ColorRGBA, Position } from "@digital-logic-sim/shared-types";

export type ChipType = "io" | "atomic" | "composite";
export type Chip = IOChip | AtomicChip | CompositeChip;

export type IOChipType = "input" | "output";
export type IOChip = InputChip | OutputChip;

export type ChipInitParams = {
	color: ColorRGBA;
	position: Position;
};

export type ChipRenderState = ChipInitParams;

type BaseChipSpec<TChipType extends ChipType> = {
	chipType: TChipType;
	name: string;
	inputPins: PinSpec[];
	outputPins: PinSpec[];
};

type BaseIOChipSpec<TIOChipType extends IOChipType> = BaseChipSpec<"io"> & {
	ioChipType: TIOChipType;
};

export type InputChipSpec = BaseIOChipSpec<"input"> & {
	ChipClass: new (
		spec: InputChipSpec,
		chipInitParams: ChipInitParams,
	) => InputChip;
};

export type OutputChipSpec = BaseIOChipSpec<"output"> & {
	ChipClass: new (
		spec: OutputChipSpec,
		chipInitParams: ChipInitParams,
	) => OutputChip;
};

export type IOChipSpec = InputChipSpec | OutputChipSpec;

export type AtomicChipSpec = BaseChipSpec<"atomic"> & {
	ChipClass: new (
		chipSpec: AtomicChipSpec,
		chipInitParams: ChipInitParams,
	) => AtomicChip;
};

export type CompositeChipSpec = BaseChipSpec<"composite">;

export type ChipSpec = IOChipSpec | AtomicChipSpec | CompositeChipSpec;
