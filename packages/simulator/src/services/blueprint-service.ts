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
		this.sim.on("chip.save", () => this.saveChipAsBlueprint("Hello"));
	}

	public importBlueprint(blueprintString: string): void {
		const blueprint = JSON.parse(blueprintString) as Blueprint;

		this.sim.chipLibraryService.register({
			name: String(Math.random())[5],
			chipType: "composite",
			inputPins: [{ name: "NAND in 0" }, { name: "NAND in 1" }],
			outputPins: [{ name: "NAND out 0" }],
			blueprint,
		});

		console.log("service", blueprint);
	}

	private saveChipAsBlueprint(blueprintName: string): Blueprint {
		const internalChips = this.sim.chipManager
			.getBoardChips()
			.reduce((acc, chip) => {
				if (!EntityUtils.isIOChip(chip)) {
					acc.push(this.serializeChip(chip));
				}
				return acc;
			}, [] as BlueprintChip[]);

		const internalWires = this.sim.wireManager.getBoardWires().reduce((acc, wire) => {
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
			this.createIOPinMappings();

		const blueprint: Blueprint = {
			chips: internalChips,
			wires: internalWires,
			inputMappings,
			outputMappings,
		};

		this.sim.chipLibraryService.register({
			name: blueprintName,
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

	/**
	 * Creates the mapping to determine which
	 * internal pin represents this external pin of the composite.
	 */
	private createIOPinMappings(): {
		inputMappings: BlueprintPinMapping[];
		outputMappings: BlueprintPinMapping[];
	} {
		const inputMappings: BlueprintPinMapping[] = [];
		const outputMappings: BlueprintPinMapping[] = [];

		this.sim.chipManager.getBoardChips().forEach((chip) => {
			if (EntityUtils.isInputChip(chip)) {
				inputMappings.push(this.getIOBlueprintPinMapping(chip));
			}

			if (EntityUtils.isOutputChip(chip)) {
				outputMappings.push(
					this.getIOBlueprintPinMapping(chip),
				);
			}
		});

		return { inputMappings, outputMappings };
	}

	// TODO: this method should work for 1:n input wires and n:1 output wires
	// TODO: (currently it only works for 1:1)
	private getIOBlueprintPinMapping(
		ioChip: IOChip,
	): BlueprintPinMapping {
		const ioPin = ioChip.getPin();

		const connectedWires = this.sim.wireManager.getOutgoingWiresFromPin(ioPin.id);

		if (connectedWires.length <= 0) {
			throw new Error("IO pin has no connected wires");
		}

		const wire = connectedWires[0];

		const internalChip = (ioChip.ioChipType === "input" ? wire.endPin : wire.startPin).chip

		if (!internalChip || EntityUtils.isIOChip(internalChip)) {
			throw new Error("IO pin not connected to a non-IO chip");
		}
		

		return {
			externalPin: ioChip.externalPinName,
			internalChipId: internalChip.id,
			internalPinName: internalChip.spec.name,
		};
	}
}
