import type { Chip, ChipRenderState, ChipSpec } from "../entities/chips";
import type { Wire, WireRenderState, WireSpec } from "../entities/wire";
import type { Simulator } from "../simulator";
import { BaseService } from "./base-service";

export type Blueprint = {
	chips: BlueprintChip[];
	wires: BlueprintWire[];
};

type BlueprintPin = {
	id: string;
	name: string;
};

type BlueprintChip = {
	spec: ChipSpec;
	renderState: ChipRenderState;
	inputPins: BlueprintPin[];
	outputPins: BlueprintPin[];
};

type BlueprintWire = {
	spec: WireSpec;
	renderState: WireRenderState;
};

export class BlueprintService extends BaseService {
	constructor(sim: Simulator) {
		super(sim);

		this.init();
	}

	private init(): void {
		this.sim.on("chip.save", () => this.saveBlueprint());
	}

	public loadBlueprint(blueprint: Blueprint): void {}

	private saveBlueprint(): Blueprint {
		const blueprint: Blueprint = {
			chips: this.sim.chipManager.chips.map((chip) => this.serializeChip(chip)),
			wires: this.sim.wireManager.wires.map((wire) => this.serializeWire(wire)),
		};

		this.sim.chipLibraryService.add({
			name: "NAND",
			chipType: "composite",
			blueprint,
			inputPins: [{ name: "NAND in 0" }, { name: "NAND in 1" }],
			outputPins: [{ name: "NAND out 0" }],
		});

		console.log("blueprint", blueprint);

		return blueprint;
	}

	private serializeChip(chip: Chip): BlueprintChip {
		// atomic chip
		return {
			spec: chip.spec,
			renderState: chip.renderState,
			inputPins: chip.inputPins.map((pin) => ({
				id: pin.id,
				name: pin.spec.name,
			})),
			outputPins: chip.inputPins.map((pin) => ({
				id: pin.id,
				name: pin.spec.name,
			})),
		};
	}

	private serializeWire(wire: Wire): BlueprintWire {
		return {
			spec: wire.spec,
			renderState: wire.renderState,
		};
	}
}
