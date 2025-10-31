import { entityIdService } from "../entity-id-service";
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

	protected readonly inputPins: Pin[];
	protected readonly outputPins: Pin[];

	constructor(spec: ChipSpec) {
		const chipId = entityIdService.getId(); // TODO, should not be only inputChipId

		super({
			id: chipId,
			type: "chip",
		});

		this.spec = spec;

		this.inputPins = spec.inputPins.map(
			(pinSpec, idx) => new Pin(pinSpec, `${chipId}.in.${idx}`), // TODO: is this the best way for id?
		);
		this.outputPins = spec.outputPins.map(
			(pinSpec, idx) => new Pin(pinSpec, `${chipId}.out.${idx}`),
		);
	}

	public getPin(pinType: PinType, index: number): Pin | undefined {
		const pins = pinType === "in" ? this.inputPins : this.outputPins;
		return pins[index];
	}

	public setOutputPins(values: boolean[]): void {
		this.outputPins.forEach((_, idx) => {
			this.outputPins[idx].nextValue = values[idx];
		});
	}

	public commitPinValues(): void {
		[...this.inputPins, ...this.outputPins].forEach((pin) => {
			pin.commitValue();
		});
	}

	public abstract execute(): void;
}
