import type {
	Chip,
	ChipRenderState,
	ChipSpec,
	IOChip,
} from "../entities/chips";
import { PinSpec } from "../entities/pin";
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

type BlueprintPin = Pick<PinSpec, "label" | "name">;

type BlueprintChip = {
	id: string;
	spec: Pick<ChipSpec, "name" | "chipType"> & {
		inputPins: BlueprintPin[];
		outputPins: BlueprintPin[];
	};
	renderState: ChipRenderState;
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

	public importBlueprint(name: string, blueprintString: string): void {
		const blueprint = JSON.parse(blueprintString) as Blueprint;
		console.log("service", blueprint);

		this.sim.chipLibraryService.register({
			name,
			chipType: "composite",
			inputPins: [{ name: "NAND in 0" }, { name: "NAND in 1" }],
			outputPins: [{ name: "NAND out 0" }],
			blueprint,
		});
	}

	private saveChipAsBlueprint(blueprintName: string): Blueprint {
		const internalChips = this.sim.chipManager
			.getBoardChips()
			.reduce((acc, chip, idx) => {
				if (!EntityUtils.isIOChip(chip)) {
					acc.push(this.serializeChip(chip, idx.toString()));
				}
				return acc;
			}, [] as BlueprintChip[]);

		const internalWires = this.sim.wireManager
			.getBoardWires()
			.reduce((acc, wire) => {
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

		const { inputMappings, outputMappings } = this.createIOPinMappings();

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

	private serializeChip(chip: Chip, blueprintChipId: string): BlueprintChip {
		// atomic chip
		return {
			id: blueprintChipId,
			spec: {
				chipType: chip.spec.chipType,
				name: chip.spec.name,
				inputPins: chip.inputPins.map((pin) => ({
					name: pin.spec.name,
					label: pin.spec.label,
				})),
				outputPins: chip.outputPins.map((pin) => ({
					name: pin.spec.name,
					label: pin.spec.label,
				})),
			},
			renderState: chip.renderState,
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
				outputMappings.push(this.getIOBlueprintPinMapping(chip));
			}
		});

		return { inputMappings, outputMappings };
	}

	// TODO: this method should work for 1:n input wires and n:1 output wires
	// TODO: (currently it only works for 1:1)
	private getIOBlueprintPinMapping(ioChip: IOChip): BlueprintPinMapping {
		const ioPin = ioChip.getPin();

		const connectedWires =
			ioChip.ioChipType === "input"
				? this.sim.wireManager.getOutgoingWires(ioPin.id)
				: this.sim.wireManager.getIncomingWires(ioPin.id);

		if (connectedWires.length <= 0) {
			throw new Error("IO pin has no connected wires");
		}

		const wire = connectedWires[0];

		const internalChip = (
			ioChip.ioChipType === "input" ? wire.endPin : wire.startPin
		).chip;

		if (!internalChip || EntityUtils.isIOChip(internalChip)) {
			throw new Error("IO pin not connected to a non-IO chip");
		}

		return {
			externalPin: ioChip.externalPinName,
			internalChipId: internalChip.id,
			internalPinName:
				ioChip.ioChipType === "input"
					? wire.endPin.spec.name
					: wire.startPin.spec.name,
		};
	}
}
