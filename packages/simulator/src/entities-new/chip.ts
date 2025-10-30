import { entityIdService } from "../entities/services";
import type { AtomicChip } from "./atomic-chip";
import { Entity } from "./entity";
import { Pin, type PinType, type PinSpec } from "./pin";

export type ChipType = "input" | "output" | "atomic" | "composite";

type BaseChipSpec = {
	name: string;
	inputPins: PinSpec[];
	outputPins: PinSpec[];
};

export type AtomicChipSpec = BaseChipSpec & {
	type: "atomic";
	ChipClass: new (spec: AtomicChipSpec) => AtomicChip;
};

export type CompositeChipSpec = BaseChipSpec & {
	type: "composite";
};

export type ChipSpec = AtomicChipSpec | CompositeChipSpec;

export abstract class Chip extends Entity {
	public readonly spec: ChipSpec;

	private readonly inputPins: Pin[];
	private readonly outputPins: Pin[];

	constructor(spec: ChipSpec) {
		super({ id: entityIdService.getId(), type: "chip" });

		this.spec = spec;
		this.inputPins = spec.inputPins.map((pinSpec) => new Pin(pinSpec));
		this.outputPins = spec.outputPins.map((pinSpec) => new Pin(pinSpec));
	}

	public getPin(mode: PinType, index: number): Pin | undefined {
		const pins = mode === "in" ? this.inputPins : this.outputPins;
		return pins[index];
	}

	public abstract execute(inputs: boolean[]): boolean[];
}
