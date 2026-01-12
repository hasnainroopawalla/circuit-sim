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
import { InvalidWireConnectionError } from "../../errors";

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

	private saveBlueprint(blueprintName: string): Blueprint {
		const definitions: Blueprint["definitions"] = {};
		const visited = new Set<string>();

		const boardCompositeDefinition = this.getBoardCompositeDefinition();
		definitions[blueprintName] = boardCompositeDefinition;

		visited.add(blueprintName);

		boardCompositeDefinition.chips.forEach((chip) => {
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

		return blueprint;
	}

	private collectCompositeDefinitions(
		compositeChipName: string,
		definitions: Blueprint["definitions"],
		visited: Set<string>,
	) {
		if (visited.has(compositeChipName)) {
			return;
		}

		visited.add(compositeChipName);

		const compositeChipFactory = this.sim.chipLibraryService.getChipFactory({
			kind: "composite",
			name: compositeChipName,
		});

		definitions[compositeChipName] = compositeChipFactory.spec.definition;

		compositeChipFactory.spec.definition.chips.forEach((chip) => {
			if (chip.spec.chipType === "composite") {
				this.collectCompositeDefinitions(chip.spec.name, definitions, visited);
			}
		});
	}

	private getBoardCompositeDefinition(): CompositeDefinition {
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

		return connectedWires.map((wire) => {
			const internalPin =
				ioChip.ioChipType === "input" ? wire.endPin : wire.startPin;
			const internalChip = internalPin.chip;

			if (EntityUtils.isIOChip(internalChip)) {
				throw new InvalidWireConnectionError(wire.startPin, wire.endPin);
			}

			return {
				internalChipId: internalChip.id,
				internalPinName: internalPin.spec.name,
			};
		});
	}

	private getExternalPinLabel(
		ioChip: IOChip,
		pinMappings: IOMapping,
		ioChipType: IOChipType,
	): string {
		// prioritize using the pin label that was added by the user
		const customPinLabel = ioChip.getPin().pinInitParams.label;
		if (customPinLabel) {
			return customPinLabel;
		}

		return `${ioChipType === "input" ? "IN" : "OUT"} ${Object.keys(pinMappings).length + 1}`;
	}
}
