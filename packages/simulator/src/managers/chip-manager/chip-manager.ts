import {
	type Chip,
	type ChipInitParams,
	type IOChip,
	type AtomicChip,
	type ChipSpawnOptions,
	type IOChipType,
	type IOChipInitParams,
	CompositeChip,
} from "../../entities/chips";
import { EntityUtils } from "../../entities/utils";
import { entityIdService } from "../../entity-id-service";
import type {
	AtomicChipFactory,
	IOChipFactory,
} from "../../services/chip-library-service/builtin-registry";
import type {
	ChipFactory,
	ChipFromFactory,
	CompositeChipFactory,
} from "../../services/chip-library-service";
import type { Simulator } from "../../simulator";
import { didAnyChange } from "../../utils";
import { BaseManager } from "../base-manager";
import { CompositeChipSpawner } from "./composite-chip-spawner";

type ChipInitParamsFromFactory<T extends ChipFactory> = T extends IOChipFactory
	? IOChipInitParams
	: ChipInitParams;

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

	public spawnChip<T extends ChipFactory>(
		chipFactory: T,
		chipInitParams: ChipInitParamsFromFactory<T>,
		opts?: ChipSpawnOptions,
	): ChipFromFactory<T> {
		const chip = this.createChip(chipFactory, chipInitParams, opts);

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

	private createChip<T extends ChipFactory>(
		chipFactory: T,
		chipInitParams: ChipInitParamsFromFactory<T>,
		opts?: ChipSpawnOptions,
	): ChipFromFactory<T> {
		switch (chipFactory.kind) {
			case "io":
				return this.createIOChip(
					chipFactory,
					chipInitParams,
					opts,
				) as ChipFromFactory<T>;
			case "atomic":
				return this.createAtomicChip(
					chipFactory,
					chipInitParams,
					opts,
				) as ChipFromFactory<T>;
			case "composite":
				return this.createCompositeChip(
					chipFactory,
					chipInitParams,
					opts,
				) as ChipFromFactory<T>;
		}
	}

	private createIOChip(
		chipFactory: IOChipFactory,
		chipInitParams: ChipInitParams,
		opts?: ChipSpawnOptions,
	): IOChip {
		chipFactory;
		return new chipFactory.ChipClass(
			{
				...chipInitParams,
				externalPinName: this.getExternalPinName(
					chipFactory.ChipClass.spec.ioChipType,
				),
			},
			opts,
		);
	}

	private createAtomicChip(
		chipFactory: AtomicChipFactory,
		chipInitParams: ChipInitParams,
		opts?: ChipSpawnOptions,
	): AtomicChip {
		return new chipFactory.ChipClass(chipInitParams, opts);
	}

	private createCompositeChip(
		chipFactory: CompositeChipFactory,
		chipInitParams: ChipInitParams,
		opts?: ChipSpawnOptions,
	): CompositeChip {
		return new CompositeChip(chipFactory.spec, chipInitParams, opts);
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
