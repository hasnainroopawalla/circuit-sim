import type {
	Chip,
	ChipRenderState,
	ChipSpec,
	ChipType,
	CompositeChip,
} from "../entities/chips";
import type { Wire, WireRenderState, WireSpec } from "../entities/wire";
import type { Simulator } from "../simulator";
import { BaseService } from "./base-service";

type Blueprint = {
	chips: BlueprintChip[];
	wires: BlueprintWire[];
};

type BlueprintChip = {
	chipType: ChipType;
	renderState: ChipRenderState;
	spec: ChipSpec;
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
		this.sim.on("chip.save", () => this.saveChipAsBlueprint());
	}

	private saveChipAsBlueprint(): Blueprint {
		const blueprint: Blueprint = {
			chips: this.sim.chipManager.chips.map((chip) => this.serializeChip(chip)),
			wires: this.sim.wireManager.wires.map((wire) => this.serializeWire(wire)),
		};

		console.log(blueprint);
		return blueprint;
	}

	private serializeChip(chip: Chip): BlueprintChip {
		// atomic chip
		return {
			chipType: chip.chipType,
			renderState: chip.renderState,
			spec: chip.spec,
		};
	}

	private serializeWire(wire: Wire): BlueprintWire {
		return {
			spec: wire.spec,
			renderState: wire.renderState,
		};
	}
}
