import { entityIdService } from "../entity-id-service";
import { didAnyChange } from "../utils";
import type { AtomicChip } from "./atomic-chip";
import { Entity } from "./entity";
import type { IOChip } from "./io-chip";
import { Pin, type PinType, type PinSpec } from "./pin";

type ChipType = "io" | "atomic" | "composite";

type BaseChipSpec<TChipType extends ChipType> = {
	type: TChipType;
	name: string;
	inputPins: PinSpec[];
	outputPins: PinSpec[];
};

export type IOChipSpec = BaseChipSpec<"io"> & {
	ChipClass: new (spec: IOChipSpec) => IOChip;
};

export type AtomicChipSpec = BaseChipSpec<"atomic"> & {
	ChipClass: new (spec: AtomicChipSpec) => AtomicChip;
};

export type CompositeChipSpec = BaseChipSpec<"composite">;

export type ChipSpec = IOChipSpec | AtomicChipSpec | CompositeChipSpec;

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

	/**
	 * Returns true if any pin's nextValue has changed.
	 */
	public setOutputPins(values: boolean[]): boolean {
		return didAnyChange(this.outputPins, (_, idx) => {
			if (this.outputPins[idx].nextValue !== values[idx]) {
				this.outputPins[idx].nextValue = values[idx];
				return true;
			}
			return false;
		});

		// let changed = false;

		// this.outputPins.forEach((_, idx) => {
		// 	if (this.outputPins[idx].nextValue !== values[idx]) {
		// 		this.outputPins[idx].nextValue = values[idx];
		// 		changed = true;
		// 	}
		// });

		// return changed;
	}

	public commitPinValues(): boolean {
		return didAnyChange([...this.inputPins, ...this.outputPins], (pin) =>
			pin.commitValue(),
		);
	}

	/**
	 * Returns true if any pin's nextValue has changed.
	 */
	public abstract execute(): boolean;
}
