import type { Position } from "../../types";
import type { PinSpec } from "../pin";
import type { AtomicChip } from "./atomic-chip";
import type { IOChip } from "./io-chip";

type ChipType = "io" | "atomic" | "composite";

export type ChipRenderSpec = {
	position: Position;
	color: string;
};

type BaseChipSpec<TChipType extends ChipType> = {
	type: TChipType;
	name: string;
	inputPins: PinSpec[];
	outputPins: PinSpec[];
};

export type IOChipSpec = BaseChipSpec<"io"> & {
	ChipClass: new (chipSpec: IOChipSpec, renderSpec: ChipRenderSpec) => IOChip;
};

export type AtomicChipSpec = BaseChipSpec<"atomic"> & {
	ChipClass: new (
		chipSpec: AtomicChipSpec,
		renderSpec: ChipRenderSpec,
	) => AtomicChip;
};

export type CompositeChipSpec = BaseChipSpec<"composite">;

export type ChipSpec = IOChipSpec | AtomicChipSpec | CompositeChipSpec;
