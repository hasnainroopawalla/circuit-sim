import type { ColorRGBA, Position } from "@digital-logic-sim/render-engine";
import type { PinSpec } from "../pin";
import type { AtomicChip } from "./atomic-chip";
import type { InputChip, OutputChip } from "./io-chip";
import type { CompositeChip } from "./composite-chip";

export type ChipType = "io" | "atomic" | "composite";
export type Chip = IOChip | AtomicChip | CompositeChip;

export type IOChipType = "input" | "output";
export type IOChip = InputChip | OutputChip;

export type ChipRenderSpec = {
	position: Position;
	color: ColorRGBA;
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

export type InputChipSpec = BaseIOChipSpec<"input"> & {
	ChipClass: new (spec: InputChipSpec, renderSpec: ChipRenderSpec) => InputChip;
};

export type OutputChipSpec = BaseIOChipSpec<"output"> & {
	ChipClass: new (
		spec: OutputChipSpec,
		renderSpec: ChipRenderSpec,
	) => OutputChip;
};

export type IOChipSpec = InputChipSpec | OutputChipSpec;

export type AtomicChipSpec = BaseChipSpec<"atomic"> & {
	ChipClass: new (
		chipSpec: AtomicChipSpec,
		renderSpec: ChipRenderSpec,
	) => AtomicChip;
};

export type CompositeChipSpec = BaseChipSpec<"composite">;

export type ChipSpec = IOChipSpec | AtomicChipSpec | CompositeChipSpec;
