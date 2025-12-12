import type {
	Chip,
	ChipRenderState,
	ChipSpec,
	ChipType,
	CompositeChip,
	CompositeChipSpec,
} from "../entities/chips";
import type { Wire, WireRenderState, WireSpec } from "../entities/wire";
import type { Simulator } from "../simulator";
import { BaseService } from "./base-service";

const NAND_BLUEPRINT = `{"chips":[{"spec":{"name":"INPUT","chipType":"io","ioChipType":"input","inputPins":[],"outputPins":[{"name":"out 0"}]},"renderState":{"color":{"r":0,"g":0,"b":0,"a":1},"position":{"x":1.6,"y":1.3}}},{"spec":{"name":"INPUT","chipType":"io","ioChipType":"input","inputPins":[],"outputPins":[{"name":"out 0"}]},"renderState":{"color":{"r":0,"g":0,"b":0,"a":1},"position":{"x":1.6,"y":0.8}}},{"spec":{"name":"AND","atomicChipType":"AND","chipType":"atomic","inputPins":[{"name":"and in 0"},{"name":"and in 1"}],"outputPins":[{"name":"and out 0"}]},"renderState":{"color":{"r":0,"g":0,"b":0,"a":1},"position":{"x":0.3,"y":1}}},{"spec":{"name":"NOT","atomicChipType":"NOT","chipType":"atomic","inputPins":[{"name":"not in 0"}],"outputPins":[{"name":"not out 0"}]},"renderState":{"color":{"r":0,"g":0,"b":0,"a":1},"position":{"x":-1,"y":1}}},{"spec":{"name":"OUTPUT","chipType":"io","ioChipType":"output","inputPins":[{"name":"in 0"}],"outputPins":[]},"renderState":{"color":{"r":0,"g":0,"b":0,"a":1},"position":{"x":-2,"y":1}}}],"wires":[{"spec":{"startPinId":"0.out.0","endPinId":"2.in.0"},"renderState":{"color":{"r":0,"g":0,"b":0,"a":1},"controlPoints":[]}},{"spec":{"startPinId":"1.out.0","endPinId":"2.in.1"},"renderState":{"color":{"r":0,"g":0,"b":0,"a":1},"controlPoints":[]}},{"spec":{"startPinId":"2.out.0","endPinId":"3.in.0"},"renderState":{"color":{"r":0,"g":0,"b":0,"a":1},"controlPoints":[]}},{"spec":{"startPinId":"3.out.0","endPinId":"4.in.0"},"renderState":{"color":{"r":0,"g":0,"b":0,"a":1},"controlPoints":[]}}]}`;

export type Blueprint = {
	chips: BlueprintChip[];
	wires: BlueprintWire[];
};

type BlueprintChip = {
	spec: ChipSpec;
	renderState: ChipRenderState;
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

	public loadBlueprint(blueprint: Blueprint): void {}

	private saveChipAsBlueprint(): Blueprint {
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

		return blueprint;
	}

	private serializeChip(chip: Chip): BlueprintChip {
		// atomic chip
		return {
			spec: chip.spec,
			renderState: chip.renderState,
		};
	}

	private serializeWire(wire: Wire): BlueprintWire {
		return {
			spec: wire.spec,
			renderState: wire.renderState,
		};
	}
}
