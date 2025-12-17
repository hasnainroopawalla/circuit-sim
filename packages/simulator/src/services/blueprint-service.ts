import type {
	Chip,
	ChipRenderState,
	ChipSpec,
	IOChip,
} from "../entities/chips";
import { EntityUtils } from "../entities/utils";
import type { Wire, WireRenderState } from "../entities/wire";
import type { Simulator } from "../simulator";
import { BaseService } from "./base-service";

export type Blueprint = {
	chips: BlueprintChip[];
	wires: BlueprintWire[];

	inputMappings: BlueprintPinMapping[];
	outputMappings: BlueprintPinMapping[];
};

type BlueprintPinMapping = {
	externalPin: string;
	internalChipId: string;
	internalPinName: string;
};

type BlueprintPin = {
	id: string;
	name: string;
};

type BlueprintChip = {
	id: string;
	spec: ChipSpec;
	renderState: ChipRenderState;
	inputPins: BlueprintPin[];
	outputPins: BlueprintPin[];
};

type BlueprintWireConnection = {
	chipId: string;
	pinName: string;
};

type BlueprintWire = {
	spec: {
		start: BlueprintWireConnection;
		end: BlueprintWireConnection;
	};
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

	public importBlueprint(blueprintString: string): void {
		const blueprint = JSON.parse(blueprintString) as Blueprint;

		this.sim.chipLibraryService.register({
			name: "NAND",
			chipType: "composite",
			inputPins: [{ name: "NAND in 0" }, { name: "NAND in 1" }],
			outputPins: [{ name: "NAND out 0" }],
			blueprint,
		});

		console.log("service", blueprint);
	}

	private saveChipAsBlueprint(): Blueprint {
		const blueprintChips = this.sim.chipManager.chips.reduce((acc, chip) => {
			if (EntityUtils.isIOChip(chip)) {
				return acc;
			}
			acc.push(this.serializeChip(chip));
			return acc;
		}, [] as BlueprintChip[]);

		const wires = this.sim.wireManager.wires.reduce((acc, wire) => {
			// omit wires that are connected to composite IO chips
			if (
				EntityUtils.isIOChip(wire.startPin.chip) ||
				EntityUtils.isIOChip(wire.endPin.chip)
			) {
				return acc;
			}
			acc.push(this.serializeWire(wire));
			return acc;
		}, [] as BlueprintWire[]);

		const { inputMappings, outputMappings } =
			this.createIOPinMappings(blueprintChips);

		const blueprint: Blueprint = {
			chips: blueprintChips,
			wires,
			inputMappings,
			outputMappings,
		};

		this.sim.chipLibraryService.register({
			name: "NAND",
			chipType: "composite",
			inputPins: [{ name: "in0" }, { name: "in1" }],
			outputPins: [{ name: "out0" }],
			blueprint,
		});

		console.log("blueprint", JSON.stringify(blueprint));

		return blueprint;
	}

	private serializeChip(chip: Chip): BlueprintChip {
		// atomic chip
		return {
			id: chip.id,
			spec: chip.spec,
			renderState: chip.renderState,
			inputPins: chip.inputPins.map((pin) => ({
				id: pin.id,
				name: pin.spec.name,
			})),
			outputPins: chip.outputPins.map((pin) => ({
				id: pin.id,
				name: pin.spec.name,
			})),
		};
	}

	private serializeWire(wire: Wire): BlueprintWire {
		return {
			spec: {
				start: {
					chipId: wire.startPin.chip.id,
					pinName: wire.startPin.spec.name,
				},
				end: {
					chipId: wire.endPin.chip.id,
					pinName: wire.endPin.spec.name,
				},
			},
			renderState: wire.renderState,
		};
	}

	private createIOPinMappings(blueprintChips: BlueprintChip[]): {
		inputMappings: BlueprintPinMapping[];
		outputMappings: BlueprintPinMapping[];
	} {
		const inputMappings: BlueprintPinMapping[] = [];
		const outputMappings: BlueprintPinMapping[] = [];

		this.sim.chipManager.chips.forEach((chip) => {
			if (EntityUtils.isInputChip(chip)) {
				inputMappings.push(this.getIOBlueprintPinMapping(chip, blueprintChips));
			}

			if (EntityUtils.isOutputChip(chip)) {
				outputMappings.push(
					this.getIOBlueprintPinMapping(chip, blueprintChips),
				);
			}
		});

		return { inputMappings, outputMappings };
	}

	private getIOBlueprintPinMapping(
		ioChip: IOChip,
		blueprintChips: BlueprintChip[],
	): BlueprintPinMapping {
		const ioWire = this.sim.wireManager.wires.find((wire) => {
			const wirePinId =
				ioChip.ioChipType === "input"
					? // outgoing from input chip
						wire.startPin.id
					: // incoming into output chip
						wire.endPin.id;

			return wirePinId === ioChip.getPin().id;
		});

		if (!ioWire) {
			throw new Error("IO wire does not exist");
		}

		const targetChip = blueprintChips.find((chip) => {
			const wirePinId =
				ioChip.ioChipType === "input"
					? ioWire.endPin.chip.id
					: ioWire.startPin.chip.id;

			return chip.id === wirePinId;
		});

		if (!targetChip) {
			throw new Error("Input connected to non-internal chip");
		}

		return {
			externalPin: ioChip.externalPinName,
			internalChipId: targetChip.id,
			internalPinName: ioWire.endPin.spec.name,
		};
	}
}
