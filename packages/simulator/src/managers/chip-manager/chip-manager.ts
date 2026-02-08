import {
	type Chip,
	type ChipInitParams,
	type IOChip,
	type AtomicChip,
	type EntitySpawnOptions,
	CompositeChip,
	ChipType,
} from "../../entities/chips";
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
import { EntityUtils } from "../../entities/utils";

export class ChipManager extends BaseManager {
	private chips: Chip[];

	private compositeChipSpawner: CompositeChipSpawner;

	constructor(sim: Simulator) {
		super(sim);

		this.compositeChipSpawner = new CompositeChipSpawner(
			this,
			this.sim.wireManager,
			this.sim.chipLibraryService,
		);
		this.chips = [];

		this.init();
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

	/**
	 * Returns a list of chips that are currently on the board (not internal to composite chips).
	 */
	public getBoardChips(): Chip[] {
		return this.chips.filter((chip) => !chip.parentCompositeId);
	}

	public getInternalChips(parentCompositeId: string): Chip[] {
		return this.chips.filter(
			(chip) => chip.parentCompositeId === parentCompositeId,
		);
	}

	public reset(): void {
		this.chips = [];
	}

	public spawnChip<T extends ChipFactory>(
		chipFactory: T,
		chipInitParams: Pick<ChipInitParams, "position">,
		opts?: EntitySpawnOptions,
	): ChipFromFactory<T> {
		const chip = this.createChip(chipFactory, chipInitParams, opts);

		const chipId = entityIdService.generateId();
		chip.setId(chipId);

		this.spawnPinsForChip(chip);

		this.chips.push(chip);

		if (EntityUtils.isCompositeChip(chip)) {
			this.compositeChipSpawner.spawn(chip);
		}

		// for inner chips that are within a composite chip,
		// we dont need to trigger an overlay update to render labels.
		if (!opts?.parentCompositeId) {
			this.sim.emit("chip.spawn.finish", {
				chipId: chip.id,
				chipName: chip.spec.name,
				chipType: chip.chipType,
				pins: [...chip.inputPins, ...chip.outputPins].map((pin) => ({
					id: pin.id,
					name: pin.spec.name,
					pinType: pin.pinType,
				})),
			});
		}

		return chip;
	}

	private init(): void {
		this.sim.on("chip.delete.start", ({ chipId }) => this.deleteChip(chipId));
	}

	private createChip<T extends ChipFactory>(
		chipFactory: T,
		chipInitParams: ChipInitParams,
		opts?: EntitySpawnOptions,
	): ChipFromFactory<T> {
		switch (chipFactory.kind) {
			case ChipType.IO:
				return this.createIOChip(
					chipFactory,
					chipInitParams,
					opts,
				) as ChipFromFactory<T>;
			case ChipType.Atomic:
				return this.createAtomicChip(
					chipFactory,
					chipInitParams,
					opts,
				) as ChipFromFactory<T>;
			case ChipType.Composite:
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
		opts?: EntitySpawnOptions,
	): IOChip {
		return new chipFactory.ChipClass(chipInitParams, opts);
	}

	private createAtomicChip(
		chipFactory: AtomicChipFactory,
		chipInitParams: ChipInitParams,
		opts?: EntitySpawnOptions,
	): AtomicChip {
		return new chipFactory.ChipClass(chipInitParams, opts);
	}

	private createCompositeChip(
		chipFactory: CompositeChipFactory,
		chipInitParams: ChipInitParams,
		opts?: EntitySpawnOptions,
	): CompositeChip {
		return new CompositeChip(chipFactory.spec, chipInitParams, opts);
	}

	private spawnPinsForChip(chip: Chip): void {
		[...chip.inputPins, ...chip.outputPins].forEach((pin) => {
			this.sim.pinManager.spawnPin(pin, chip.id);
		});
	}

	private deleteChip(chipId: string): void {
		const chipIdsToDelete = [
			...this.getChipIdsToDelete(chipId),
			// add the self chipId since it needs to be deleted too
			chipId,
		];

		// delete wires including nested wires
		chipIdsToDelete.forEach((chipId) => {
			this.sim.wireManager.deleteWiresForChip(chipId);
		});

		// remove the chips from the list including self
		this.chips = this.chips.filter(
			(chip) => !chipIdsToDelete.includes(chip.id),
		);

		this.sim.emit("chip.delete.finish", {
			chipId,
		});
	}

	// note: recursive
	private getChipIdsToDelete(
		chipId: string,
		chipIdsToDelete: string[] = [],
	): string[] {
		if (!chipId) {
			return chipIdsToDelete;
		}

		this.getInternalChips(chipId).forEach((chip) => {
			if (chip.parentCompositeId === chipId) {
				chipIdsToDelete.push(chip.id);
				this.getChipIdsToDelete(chip.id, chipIdsToDelete);
			}
		});

		return chipIdsToDelete;
	}
}
