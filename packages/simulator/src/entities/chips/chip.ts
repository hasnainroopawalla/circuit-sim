import {
	renderEngineConfig,
	type Position,
	type RectDimensions,
} from "@digital-logic-sim/render-engine";
import { entityIdService } from "../../entity-id-service";
import { didAnyChange } from "../../utils";
import { BaseEntity } from "../entity";
import { Pin, type PinSpec, type PinType } from "../pin";
import type {
	ChipSpec,
	ChipRenderSpec,
	ChipType,
	Chip,
} from "./chip.interface";
import { ChipUtils } from "./chip.utils";

const chipConfig = {
	aspectRatio: 1.5,
};

type ChipSpecOf<TChipType> = Extract<ChipSpec, { chipType: TChipType }>;

export abstract class BaseChip<
	TChipType extends ChipType,
> extends BaseEntity<"chip"> {
	public spec: ChipSpecOf<TChipType>;
	public renderSpec: ChipRenderSpec;

	public inputPins: Pin[];
	public outputPins: Pin[];

	public chipType: TChipType;

	constructor(chipSpec: ChipSpecOf<TChipType>, renderSpec: ChipRenderSpec) {
		const chipId = entityIdService.generateId();

		super({
			id: chipId,
			entityType: "chip",
		});

		this.chipType = chipSpec.chipType;

		this.inputPins = this.createPins(chipSpec.inputPins, "in", chipId);
		this.outputPins = this.createPins(chipSpec.outputPins, "out", chipId);

		this.spec = chipSpec;
		this.renderSpec = {
			color: renderSpec.color,
			position: renderSpec.position,
			dimensions: this.getDimensions(),
		};
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
		this.renderSpec.position = position;
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

	public getPinOffset(pinType: PinType): Position {
		const [numInputPins, numOutputPins] = [
			this.spec.inputPins.length,
			this.spec.outputPins.length,
		];

		const [height, width] = [
			this.renderSpec.dimensions.height,
			this.renderSpec.dimensions.width,
		];

		const maxPins = Math.max(numInputPins, numOutputPins);

		switch (pinType) {
			case "in":
				return {
					x: this.renderSpec.position.x + width,
					y:
						this.renderSpec.position.y +
						height -
						renderEngineConfig.pinSize *
							(2 + (3 * (maxPins - numInputPins)) / 2),
				};
			case "out":
				return {
					x: this.renderSpec.position.x - width,
					y:
						this.renderSpec.position.y +
						height -
						renderEngineConfig.pinSize *
							(2 + (3 * (maxPins - numOutputPins)) / 2),
				};
		}
	}

	public getPinPosition(pinIdx: number, pinType: PinType): Position {
		const { inputPinOffset, outputPinOffset } = ChipUtils.getPinOffsets(this);

		const pinOffset = pinType === "in" ? inputPinOffset : outputPinOffset;

		return {
			x: pinOffset.x,
			y: pinOffset.y - renderEngineConfig.pinSize * 3 * pinIdx,
		};
	}

	/**
	 * Returns true if any pin's nextValue has changed.
	 */
	public abstract execute(): boolean;

	private getDimensions(): RectDimensions {
		const [numInputPins, numOutputPins] = [
			this.spec.inputPins.length,
			this.spec.outputPins.length,
		];

		const maxPins = Math.max(numInputPins, numOutputPins);

		const height =
			(maxPins * chipConfig.aspectRatio + 0.5) * renderEngineConfig.pinSize;
		const width = chipConfig.aspectRatio * height;

		return {
			height,
			width,
		};
	}
}
