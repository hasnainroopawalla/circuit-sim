import { entityIdService } from "../../entity-id-service";
import { didAnyChange } from "../../utils";
import { BaseEntity } from "../entity";
import { Pin, type PinSpec, type PinType } from "../pin";
import type {
	ChipSpec,
	ChipInitParams,
	ChipType,
	Chip,
	ChipRenderState,
} from "./chip.interface";
import { type ChipLayout, ChipLayoutFactory } from "./chip-layout-factory";
import type { Position } from "@digital-logic-sim/shared-types";

type ChipSpecOf<TChipType> = Extract<ChipSpec, { chipType: TChipType }>;

export abstract class BaseChip<
	TChipType extends ChipType,
> extends BaseEntity<"chip"> {
	public spec: ChipSpecOf<TChipType>;
	public renderState: ChipRenderState;

	public inputPins: Pin[];
	public outputPins: Pin[];

	public chipType: TChipType;

	public layout: ChipLayout;

	constructor(chipSpec: ChipSpecOf<TChipType>, chipInitParams: ChipInitParams) {
		const chipId = entityIdService.generateId();

		super({
			id: chipId,
			entityType: "chip",
		});

		this.chipType = chipSpec.chipType;

		this.inputPins = this.createPins(chipSpec.inputPins, "in", chipId);
		this.outputPins = this.createPins(chipSpec.outputPins, "out", chipId);

		this.renderState = {
			color: chipInitParams.color,
			position: chipInitParams.position,
		};

		this.spec = chipSpec;

		this.layout = new ChipLayoutFactory(this);
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

	public setPosition(position: Position): void {
		this.renderState.position = position;
	}

	private createPins(
		pinSpec: PinSpec[],
		pinType: PinType,
		chipId: string,
	): Pin[] {
		return pinSpec.map(
			(pinSpec, idx) =>
				new Pin({
					spec: pinSpec,
					id: entityIdService.generatePinId(chipId, idx, pinType),
					pinType,
					pinIdx: idx,
					chip: this as Chip,
				}),
		);
	}

	/**
	 * Returns true if any pin's nextValue has changed.
	 */
	public abstract execute(): boolean;
}
