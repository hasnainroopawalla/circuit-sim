import type { ChipSpec } from "../entities-new/chip";
import type { Simulator } from "../simulator";
import { BaseService } from "./base-service";

export class ChipLibraryService extends BaseService {
	protected readonly chipSpecs: ChipSpec[];

	constructor(sim: Simulator) {
		super(sim);
		this.chipSpecs = [];
	}

	public add(chipSpec: ChipSpec): void {
		this.chipSpecs.push(chipSpec);
	}

	public getAll(): ChipSpec[] {
		return this.chipSpecs;
	}
}
