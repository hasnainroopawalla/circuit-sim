import { AndChip, NotChip } from "../entities/atomic-chip";
import type { ChipSpec } from "../entities/chip";
import { InputChip, OutputChip } from "../entities/io-chip";
import type { Simulator } from "../simulator";
import { BaseService } from "./base-service";

export const PRIMITIVE_CHIP_SPECS: ChipSpec[] = [
	{
		name: "INPUT",
		type: "io",
		inputPins: [],
		outputPins: [{ name: "io-in-0" }], // TODO: should not be necessary
		ChipClass: InputChip,
	},
	{
		name: "OUTPUT",
		type: "io",
		inputPins: [{ name: "io-out-0" }],
		outputPins: [],
		ChipClass: OutputChip,
	},
	{
		name: "AND",
		type: "atomic",
		inputPins: [{ name: "and-in-0" }, { name: "and-in-1" }],
		outputPins: [{ name: "and-out-0" }],
		ChipClass: AndChip,
	},
	{
		name: "NOT",
		type: "atomic",
		inputPins: [{ name: "not-in-0" }],
		outputPins: [{ name: "not-out-0" }],
		ChipClass: NotChip,
	},
];

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
}
