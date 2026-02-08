import type { PinSpec } from "../pin";
import type { AtomicChip } from "./atomic-chip";
import type { InputChip, OutputChip } from "./io-chip";
import type { CompositeChip } from "./composite-chip";
import type { ColorRGBA, Position } from "@digital-logic-sim/shared-types";
import type { CompositeDefinition } from "../../services/blueprint-service";

export enum AtomicChipType {
	And = "And",
	Or = "Or",
	Not = "Not",
}

export enum ChipType {
	IO = "IO",
	Atomic = "Atomic",
	Composite = "Composite",
}
export type Chip = IOChip | AtomicChip | CompositeChip;

export enum IOChipType {
	Input = "Input",
	Output = "Output",
}
export type IOChip = InputChip | OutputChip;

export type ChipRenderState = ChipInitParams & {
	color: ColorRGBA;
};

export type EntitySpawnOptions = {
	parentCompositeId?: string;
};

export type ChipInitParams = {
	position: Position;
};

type BaseChipSpec<TChipType extends ChipType> = {
	chipType: TChipType;
	inputPins: PinSpec[];
	outputPins: PinSpec[];
	color: ColorRGBA;
};

type BaseIOChipSpec<TIOChipType extends IOChipType> =
	BaseChipSpec<ChipType.IO> & {
		ioChipType: TIOChipType;
		name: IOChipType;
	};

export type InputChipSpec = BaseIOChipSpec<IOChipType.Input>;
export type OutputChipSpec = BaseIOChipSpec<IOChipType.Output>;
export type IOChipSpec = InputChipSpec | OutputChipSpec;

export type AtomicChipSpec = BaseChipSpec<ChipType.Atomic> & {
	atomicChipType: AtomicChipType;
	name: AtomicChipType;
};

export type CompositeChipSpec = BaseChipSpec<ChipType.Composite> & {
	name: string;
	definition: CompositeDefinition;
};

export type ChipSpec = IOChipSpec | AtomicChipSpec | CompositeChipSpec;
