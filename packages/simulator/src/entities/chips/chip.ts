import { didAnyChange } from "../../utils";
import { BaseEntity } from "../entity";
import { Pin, type PinSpec, type PinType } from "../pin";
import type {
	ChipSpec,
	ChipInitParams,
	ChipType,
	Chip,
	ChipRenderState,
	EntitySpawnOptions,
} from "./chip.interface";
import { type ChipLayout, ChipLayoutFactory } from "./chip-layout-factory";
import type { ColorRGBA, Position } from "@digital-logic-sim/shared-types";
import { PinLengthMismatchError } from "../../errors";

type ChipSpecOf<TChipType> = Extract<ChipSpec, { chipType: TChipType }>;

export abstract class BaseChip<
	TChipType extends ChipType,
> extends BaseEntity<"chip"> {
	public spec: ChipSpecOf<TChipType>;

	public inputPins: Pin[];
	public outputPins: Pin[];

	public parentCompositeId: string;

	public chipType: TChipType;

	public layout: ChipLayout;

	private renderState: ChipRenderState;

	constructor(
		chipSpec: ChipSpecOf<TChipType>,
		chipInitParams: ChipInitParams,
		opts?: EntitySpawnOptions,
	) {
		super({
			entityType: "chip",
		});

		this.chipType = chipSpec.chipType;

		this.inputPins = this.createPins(chipSpec.inputPins, "in");
		this.outputPins = this.createPins(chipSpec.outputPins, "out");

		this.parentCompositeId = opts?.parentCompositeId || "";

		this.renderState = {
			color: chipInitParams.color,
			position: chipInitParams.position,
		};

		this.spec = chipSpec;

		this.layout = new ChipLayoutFactory(this.renderState, {
			numInputPins: this.spec.inputPins.length,
			numOutputPins: this.spec.outputPins.length,
		});
	}

	public getPin(name: string): Pin | undefined {
		return [...this.inputPins, ...this.outputPins].find(
			(pin) => pin.spec.name === name,
		);
	}

	/**
	 * Returns true if any pin's nextValue has changed.
	 */
	public setOutputPins(values: boolean[]): boolean {
		if (this.outputPins.length !== values.length) {
			throw new PinLengthMismatchError(values.length, this.outputPins.length);
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

	public setPosition(position: Position): void {
		this.renderState.position = position;
	}

	private createPins(pinSpec: PinSpec[], pinType: PinType): Pin[] {
		// TODO: enforce that pin name should be unique across the chip
		return pinSpec.map(
			(pinSpec, idx) =>
				new Pin({
					spec: pinSpec,
					renderState: {},
					pinType,
					pinIdx: idx,
					chip: this as Chip,
				}),
		);
	}

	public getRenderState(): ChipRenderState {
		return {
			position: this.getPosition(),
			color: this.getColor(),
		};
	}

	protected getPosition(): Position {
		return this.renderState.position;
	}

	protected getColor(): ColorRGBA {
		return this.renderState.color;
	}

	/**
	 * Returns true if any pin's nextValue has changed.
	 */
	public abstract execute(): boolean;
}
