import type { Chip, ChipRenderState, ChipSpec } from "../entities/chips";
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

export type BlueprintPinMapping = {
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

export type BlueprintWire = {
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
		this.sim.on("chip.save", () => this.saveBlueprint());
	}

	public loadBlueprint(blueprintString: string): void {
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

	private saveBlueprint(): Blueprint {
		// TODO: Reduce
		const chips = this.sim.chipManager.chips.reduce((acc, chip) => {
			if (EntityUtils.isIOChip(chip)) {
				return acc;
			}
			acc.push(this.serializeChip(chip));
			return acc;
		}, [] as BlueprintChip[]);

		const inputMappings = this.sim.chipManager.chips.reduce((acc, chip) => {
			if (!EntityUtils.isInputChip(chip)) {
				return acc;
			}

			const outgoingWire = this.sim.wireManager.wires.find(
				(wire) => wire.startPin.id === chip.getPin().id,
			);

			if (!outgoingWire) {
				throw new Error("No out going wire");
			}

			const targetChip = chips.find(
				(c) => c.id === outgoingWire.endPin.chip.id,
			);

			if (!targetChip) {
				throw new Error("Input connected to non-internal chip");
			}

			acc.push({
				externalPin: chip.externalPinName,
				internalChipId: targetChip.id,
				internalPinName: outgoingWire.endPin.spec.name,
			});
			return acc;
		}, [] as BlueprintPinMapping[]);

		const outputMappings = this.sim.chipManager.chips.reduce((acc, chip) => {
			if (!EntityUtils.isOutputChip(chip)) {
				return acc;
			}

			const outgoingWire = this.sim.wireManager.wires.find(
				(wire) => wire.endPin.id === chip.getPin().id,
			);

			if (!outgoingWire) {
				throw new Error("No out going wire");
			}

			const targetChip = chips.find(
				(c) => c.id === outgoingWire.startPin.chip.id,
			);

			if (!targetChip) {
				throw new Error("Output connected to non-internal chip");
			}

			acc.push({
				externalPin: chip.externalPinName,
				internalChipId: targetChip.id,
				internalPinName: outgoingWire.startPin.spec.name,
			});
			return acc;
		}, [] as BlueprintPinMapping[]);

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

		const blueprint: Blueprint = {
			chips,
			wires,
			inputMappings,
			outputMappings,
		};

		console.log("blueprint", JSON.stringify(blueprint));

		this.sim.chipLibraryService.register({
			name: "NAND",
			chipType: "composite",
			inputPins: [{ name: "in0" }, { name: "in1" }],
			outputPins: [{ name: "out0" }],
			blueprint,
		});

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
}
