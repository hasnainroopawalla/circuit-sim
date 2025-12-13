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
	CompositeChip,
} from "../entities/chips";
import { entityIdService } from "../entity-id-service";
import type { Simulator } from "../simulator";
import { didAnyChange } from "../utils";
import { BaseManager } from "./base-manager";

type ChipFromSpec<T extends ChipSpec> = T extends IOChipSpec
	? IOChip
	: T extends AtomicChipSpec
		? AtomicChip
		: T extends CompositeChipSpec
			? CompositeChip
			: never;

export class ChipManager extends BaseManager {
	public readonly chips: Chip[];

	constructor(sim: Simulator) {
		super(sim);

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

		// for inner chips that are within a composite chip,
		// we dont need to trigger an overlay update to render labels.
		if (!opts?.parentCompositeId) {
			this.sim.emit("entity.spawn.finish", {
				entity: { id: chip.id, name: chip.spec.name },
			});
		}

		return chip;
	}

	public spawnCompositeChip(
		chipSpec: CompositeChipSpec,
		chipInitParams: ChipInitParams,
		opts?: ChipSpawnOptions,
	): CompositeChip {
		const compositeChip = this.spawnChip(chipSpec, chipInitParams, opts);

		chipSpec.blueprint.chips.forEach((chip) => {
			this.spawnChip(chip.spec, chip.renderState, {
				parentCompositeId: compositeChip.id,
			});
		});

		chipSpec.blueprint.wires.forEach((wire) => {
			this.sim.wireManager.spawnWire(wire.spec, wire.renderState);
		});

		return compositeChip;
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
					chipInitParams,
					opts,
				) as ChipFromSpec<TSpec>;
			case "composite":
				return this.createCompositeChip(
					chipSpec,
					chipInitParams,
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
				return new InputChipClass(chipSpec, chipInitParams, opts);
			}
			case "output": {
				const OutputChipClass =
					this.sim.chipLibraryService.getOutputChipClass();
				return new OutputChipClass(chipSpec, chipInitParams, opts);
			}
		}
	}

	private createAtomicChip(
		chipSpec: AtomicChipSpec,
		chipInitParams: ChipInitParams,
		opts?: ChipSpawnOptions,
	): AtomicChip {
		const AtomicChipClass = this.sim.chipLibraryService.getAtomicChipClass(
			chipSpec.atomicChipType,
		);
		return new AtomicChipClass(chipSpec, chipInitParams, opts);
	}

	private createCompositeChip(
		chipSpec: CompositeChipSpec,
		chipInitParams: ChipInitParams,
		opts?: ChipSpawnOptions,
	): CompositeChip {
		return new CompositeChip(chipSpec, chipInitParams, opts);
	}

	private spawnPinsForChip(chip: Chip): void {
		[...chip.inputPins, ...chip.outputPins].forEach((pin) => {
			this.sim.pinManager.spawnPin(pin, chip.id);
		});
	}
}
