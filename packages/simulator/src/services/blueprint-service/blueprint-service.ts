import type {
	BlueprintSet,
	BlueprintPinMapping,
	ChipBlueprint,
	IOMapping,
	WireBlueprint,
} from "./blueprint-service.interface";
import type { Chip, IOChip, IOChipType } from "../../entities/chips";
import { EntityUtils } from "../../entities/utils";
import type { Wire } from "../../entities/wire";
import type { Simulator } from "../../simulator";
import { BaseService } from "../base-service";

export class BlueprintService extends BaseService {
	constructor(sim: Simulator) {
		super(sim);

		this.init();
	}

	private init(): void {
		this.sim.on("sim.save-chip", () => this.saveBlueprint("Hello"));
	}

	public loadBlueprint(blueprintSet: BlueprintSet): void {
		Object.entries(blueprintSet.definitions).forEach(
			([chipName, blueprint]) => {
				this.sim.chipLibraryService.register(chipName, blueprint);
			},
		);
	}

	private saveBlueprint(blueprintName: string): void {
		const internalChips = this.sim.chipManager
			.getBoardChips()
			.reduce((acc, chip, idx) => {
				if (!EntityUtils.isIOChip(chip)) {
					acc.push(this.serializeChip(chip, idx.toString()));
				}
				return acc;
			}, [] as ChipBlueprint[]);

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
			}, [] as WireBlueprint[]);

		const { inputMappings, outputMappings } = this.createIOPinMappings();

		this.sim.chipLibraryService.register(blueprintName, {
			chips: internalChips,
			wires: internalWires,
			inputMappings,
			outputMappings,
		});

		console.log("blueprint", {
			chips: internalChips,
			wires: internalWires,
			inputMappings,
			outputMappings,
		});
	}

	private serializeChip(chip: Chip, blueprintChipId: string): ChipBlueprint {
		return {
			id: blueprintChipId,
			spec: {
				chipType: chip.spec.chipType,
				name: chip.spec.name,
			},
			renderState: chip.renderState,
		};
	}

	private serializeWire(wire: Wire): WireBlueprint {
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
		inputMappings: IOMapping;
		outputMappings: IOMapping;
	} {
		const inputMappings: IOMapping = {};
		const outputMappings: IOMapping = {};

		for (const chip of this.sim.chipManager.getBoardChips()) {
			if (!EntityUtils.isIOChip(chip)) {
				continue;
			}

			const mappings =
				chip.ioChipType === "input" ? inputMappings : outputMappings;

			const externalPinLabel = this.getExternalPinLabel(
				chip,
				mappings,
				chip.ioChipType,
			);

			mappings[externalPinLabel] = this.getPinMappingsForIOChip(chip);
		}

		return { inputMappings, outputMappings };
	}

	private getPinMappingsForIOChip(ioChip: IOChip): BlueprintPinMapping[] {
		const ioPin = ioChip.getPin();

		const connectedWires =
			ioChip.ioChipType === "input"
				? this.sim.wireManager.getOutgoingWires(ioPin.id)
				: this.sim.wireManager.getIncomingWires(ioPin.id);

		if (connectedWires.length <= 0) {
			throw new Error("IO pin has no connected wires");
		}

		return connectedWires.map((wire) => {
			const internalChip = (
				ioChip.ioChipType === "input" ? wire.endPin : wire.startPin
			).chip;

			if (!internalChip || EntityUtils.isIOChip(internalChip)) {
				throw new Error("IO pin not connected to a non-IO chip");
			}

			return {
				internalChipId: internalChip.id,
				internalPinName:
					ioChip.ioChipType === "input"
						? wire.endPin.spec.name
						: wire.startPin.spec.name,
			};
		});
	}

	private getExternalPinLabel(
		ioChip: IOChip,
		pinMappings: IOMapping,
		ioChipType: IOChipType,
	): string {
		// prioritize using the pin label that was added by the user
		const customPinLabel = ioChip.getPin().renderState.label;
		if (customPinLabel) {
			return customPinLabel;
		}

		return `${ioChipType === "input" ? "IN" : "OUT"} ${Object.keys(pinMappings).length + 1}`;
	}
}
