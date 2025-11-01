import { entityIdService } from "../../entity-id-service";
import { didAnyChange } from "../../utils";
import { Entity } from "../entity";
import { Pin, type PinType } from "../pin";
import type { ChipSpec, ChipRenderSpec } from "./chip.interface";

export abstract class Chip extends Entity {
	public readonly spec: ChipSpec;
	public readonly renderSpec: ChipRenderSpec;

	protected readonly inputPins: Pin[];
	protected readonly outputPins: Pin[];

	constructor(chipSpec: ChipSpec, renderSpec: ChipRenderSpec) {
		const chipId = entityIdService.getId(); // TODO, should not be only inputChipId

		super({
			id: chipId,
			type: "chip",
		});

		this.spec = chipSpec;
		this.renderSpec = renderSpec;

		this.inputPins = chipSpec.inputPins.map(
			(pinSpec, idx) => new Pin(pinSpec, `${chipId}.in.${idx}`), // TODO: is this the best way for id?
		);
		this.outputPins = chipSpec.outputPins.map(
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
		if (this.outputPins.length !== values.length) {
			throw new Error("Pin lengths dont match.");
		}

		return didAnyChange(this.outputPins, (_, idx) => {
			if (this.outputPins[idx].nextValue !== values[idx]) {
				this.outputPins[idx].nextValue = values[idx];
				return true;
			}
			return false;
		});
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
