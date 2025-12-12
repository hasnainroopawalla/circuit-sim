import {
	type ChipSpec,
	type AtomicChipType,
	InputChip,
	OutputChip,
} from "../../entities/chips";
import type { Simulator } from "../../simulator";
import { BaseService } from "../base-service";
import {
	type AtomicChipClass,
	type IOChipClass,
	PRIMITIVE_CHIP_SPECS,
	ATOMIC_CHIPS_MAP,
} from "./chip-library";

export class ChipLibraryService extends BaseService {
	protected readonly chipSpecs: ChipSpec[];

	constructor(sim: Simulator) {
		super(sim);
		this.chipSpecs = PRIMITIVE_CHIP_SPECS;
	}

	public add(chipSpec: ChipSpec): void {
		// check if duplicate chip spec name
		if (
			this.chipSpecs.some(
				(libraryChipSpec) => libraryChipSpec.name === chipSpec.name,
			)
		) {
			return;
		}

		this.chipSpecs.push(chipSpec);
	}

	public getAll(): ChipSpec[] {
		return this.chipSpecs;
	}

	public getChipSpecByName(chipSpecName: string): ChipSpec | undefined {
		return this.chipSpecs.find(
			(libraryChipSpec) => libraryChipSpec.name === chipSpecName,
		);
	}

	public getAtomicChipClass(atomicChipName: AtomicChipType): AtomicChipClass {
		return ATOMIC_CHIPS_MAP[atomicChipName];
	}

	public getInputChipClass(): IOChipClass<"input"> {
		return InputChip;
	}

	public getOutputChipClass(): IOChipClass<"output"> {
		return OutputChip;
	}
}
