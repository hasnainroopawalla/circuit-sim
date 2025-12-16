import {
	type Chip,
	type ChipSpec,
	type ChipInitParams,
	type IOChipSpec,
	type IOChip,
	type AtomicChipSpec,
	type AtomicChip,
	type CompositeChipSpec,
	type ChipSpawnOptions,
	type AtomicChipInitParams,
	type CompositeChipInitParams,
	type InputChip,
	type OutputChip,
	type IOChipType,
	CompositeChip,
} from "../../entities/chips";
import { EntityUtils } from "../../entities/utils";
import { entityIdService } from "../../entity-id-service";
import type { Simulator } from "../../simulator";
import { didAnyChange } from "../../utils";
import { BaseManager } from "../base-manager";
import { CompositeChipSpawner } from "./composite-chip-spawner";

type ChipFromSpec<T extends ChipSpec> = T extends IOChipSpec
	? T["ioChipType"] extends "input"
		? InputChip
		: OutputChip
	: T extends AtomicChipSpec
		? AtomicChip
		: T extends CompositeChipSpec
			? CompositeChip
			: never;

export class ChipManager extends BaseManager {
	public readonly chips: Chip[];

	private compositeChipSpawner: CompositeChipSpawner;

	constructor(sim: Simulator) {
		super(sim);

		this.compositeChipSpawner = new CompositeChipSpawner(
			this,
			this.sim.wireManager,
			this.sim.chipLibraryService,
		);
		this.chips = [];
	}

	public executeChips(): boolean {
		return didAnyChange(this.chips, (chip) => chip.execute());
	}

	public getChipById(chipId: string): Chip | undefined {
		return this.chips.find((chip) => chip.id === chipId);
	}

	public commitAllPinValues(): boolean {
		return didAnyChange(this.chips, (chip) => chip.commitPinValues());
	}

	public spawnChip<TSpec extends ChipSpec>(
		chipSpec: TSpec,
		chipInitParams: ChipInitParams,
		opts?: ChipSpawnOptions,
	): ChipFromSpec<TSpec> {
		const chip = this.createChip(chipSpec, chipInitParams, opts);

		const chipId = entityIdService.generateId();
		chip.setId(chipId);

		this.spawnPinsForChip(chip);

		this.chips.push(chip);

		if (chip.chipType === "composite") {
			this.compositeChipSpawner.spawn(chip);
		}

		// for inner chips that are within a composite chip,
		// we dont need to trigger an overlay update to render labels.
		if (!opts?.parentCompositeId) {
			this.sim.emit("entity.spawn.finish", {
				entity: { id: chip.id, name: chip.spec.name },
			});
		}

		return chip;
	}

	private createChip<TSpec extends ChipSpec>(
		chipSpec: TSpec,
		chipInitParams: ChipInitParams,
		opts?: ChipSpawnOptions,
	): ChipFromSpec<TSpec> {
		switch (chipSpec.chipType) {
			case "io":
				return this.createIOChip(
					chipSpec,
					chipInitParams,
					opts,
				) as ChipFromSpec<TSpec>;
			case "atomic":
				return this.createAtomicChip(
					chipSpec,
					chipInitParams as AtomicChipInitParams,
					opts,
				) as ChipFromSpec<TSpec>;
			case "composite":
				return this.createCompositeChip(
					chipSpec,
					chipInitParams as CompositeChipInitParams,
					opts,
				) as ChipFromSpec<TSpec>;
		}
	}

	private createIOChip(
		chipSpec: IOChipSpec,
		chipInitParams: ChipInitParams,
		opts?: ChipSpawnOptions,
	): IOChip {
		switch (chipSpec.ioChipType) {
			case "input": {
				const InputChipClass = this.sim.chipLibraryService.getInputChipClass();
				return new InputChipClass(
					chipSpec,
					{
						...chipInitParams,
						chipType: "io",
						externalPinName: this.getExternalPinName("input"),
					},
					opts,
				);
			}
			case "output": {
				const OutputChipClass =
					this.sim.chipLibraryService.getOutputChipClass();
				return new OutputChipClass(
					chipSpec,
					{
						...chipInitParams,
						chipType: "io",
						externalPinName: this.getExternalPinName("output"),
					},
					opts,
				);
			}
		}
	}

	private createAtomicChip(
		chipSpec: AtomicChipSpec,
		chipInitParams: AtomicChipInitParams,
		opts?: ChipSpawnOptions,
	): AtomicChip {
		const AtomicChipClass = this.sim.chipLibraryService.getAtomicChipClass(
			chipSpec.atomicChipType,
		);
		return new AtomicChipClass(chipSpec, chipInitParams, opts);
	}

	private createCompositeChip(
		chipSpec: CompositeChipSpec,
		chipInitParams: CompositeChipInitParams,
		opts?: ChipSpawnOptions,
	): CompositeChip {
		return new CompositeChip(chipSpec, chipInitParams, opts);
	}

	private spawnPinsForChip(chip: Chip): void {
		[...chip.inputPins, ...chip.outputPins].forEach((pin) => {
			this.sim.pinManager.spawnPin(pin, chip.id);
		});
	}

	private getExternalPinName(ioChipType: IOChipType): string {
		const chipCount = this.sim.chipManager.chips.filter((chip) =>
			ioChipType === "input"
				? EntityUtils.isInputChip(chip)
				: EntityUtils.isOutputChip(chip),
		).length;

		return `${ioChipType === "input" ? "IN" : "OUT"} ${chipCount}`;
	}
}
