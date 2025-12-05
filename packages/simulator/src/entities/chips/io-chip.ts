import type { Position } from "@digital-logic-sim/render-engine";
import type { Pin } from "../pin";
import { BaseChip } from "./chip";
import type {
	ChipRenderSpec,
	InputChipSpec,
	IOChipSpec,
	IOChipType,
	OutputChipSpec,
} from "./chip.interface";

type IOChipSpecOf<TIOChipType> = Extract<
	IOChipSpec,
	{ ioChipType: TIOChipType }
>;

export abstract class BaseIOChip<
	TIOChipType extends IOChipType,
> extends BaseChip<"io"> {
	public ioChipType: TIOChipType;

	constructor(
		ioChipSpec: IOChipSpecOf<TIOChipType>,
		renderSpec: ChipRenderSpec,
		initialPosition: Position,
	) {
		super(ioChipSpec, renderSpec, initialPosition);

		this.ioChipType = ioChipSpec.ioChipType;
	}

	public getPin(): Pin {
		return this.ioChipType === "input" ? this.outputPins[0] : this.inputPins[0];
	}
}

export class InputChip extends BaseIOChip<"input"> {
	private nextValue = false;

	constructor(
		chipSpec: InputChipSpec,
		renderSpec: ChipRenderSpec,
		initialPosition: Position,
	) {
		super(chipSpec, renderSpec, initialPosition);
	}

	public toggle(): void {
		this.nextValue = !this.getPin().currentValue;
	}

	public execute(): boolean {
		const pin = this.getPin();

		if (pin.nextValue !== this.nextValue) {
			pin.nextValue = this.nextValue;
			return true;
		}

		return false;
	}
}

export class OutputChip extends BaseIOChip<"output"> {
	constructor(
		chipSpec: OutputChipSpec,
		renderSpec: ChipRenderSpec,
		initialPosition: Position,
	) {
		super(chipSpec, renderSpec, initialPosition);
	}

	public execute(): boolean {
		return false; // output chip does not perform any action
	}
}
