import type {
	Blueprint,
	BlueprintPinMapping,
	ChipBlueprint,
	CompositeDefinition,
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
		this.sim.on("sim.save-chip.start", ({ chipName }) =>
			this.saveBlueprint(chipName),
		);
	}

	public loadBlueprint(blueprint: Blueprint): void {
		this.sim.chipLibraryService.register(blueprint);
	}

	private saveBlueprint(blueprintName: string): void {
		const definitions: Blueprint["definitions"] = {};
		const visited = new Set<string>();

		const rootDefinition = this.getCompositeDefinitionOfBoard();
		definitions[blueprintName] = rootDefinition;
		visited.add(blueprintName);

		rootDefinition.chips.forEach((chip) => {
			if (chip.spec.chipType === "composite") {
				this.collectCompositeDefinitions(chip.spec.name, definitions, visited);
			}
		});

		const blueprint: Blueprint = {
			root: blueprintName,
			definitions,
		};

		this.sim.chipLibraryService.register(blueprint);

		this.sim.emit("sim.save-chip.finish", undefined);
		console.log("Saved blueprint:", blueprint);
	}

	private collectCompositeDefinitions(
		compositeName: string,
		definitions: Blueprint["definitions"],
		visited: Set<string>,
	) {
		if (visited.has(compositeName)) return;
		visited.add(compositeName);

		const compositeDefinition = this.sim.blueprintContext.get(compositeName);

		if (!compositeDefinition) {
			throw new Error("Composite definition does not exist");
		}

		definitions[compositeName] = compositeDefinition;

		for (const chip of compositeDefinition.chips) {
			if (chip.spec.chipType === "composite") {
				this.collectCompositeDefinitions(chip.spec.name, definitions, visited);
			}
		}
	}

	private getCompositeDefinitionOfBoard(): CompositeDefinition {
		const internalChips = this.sim.chipManager
			.getBoardChips()
			.reduce((acc, chip) => {
				if (!EntityUtils.isIOChip(chip)) {
					acc.push(this.serializeChip(chip));
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

		return {
			chips: internalChips,
			wires: internalWires,
			inputMappings,
			outputMappings,
		};
	}

	private serializeChip(
		chip: Pick<Chip, "spec" | "id" | "renderState">,
	): ChipBlueprint {
		return {
			id: chip.id,
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
