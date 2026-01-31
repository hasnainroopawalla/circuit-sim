import type {
	Blueprint,
	ChipBlueprint,
	CompositeDefinition,
	ExternalIOPort,
	IOMapping,
	WireBlueprint,
} from "./blueprint-service.interface";
import type { Chip, IOChip, IOChipType } from "../../entities/chips";
import { EntityUtils } from "../../entities/utils";
import type { Wire } from "../../entities/wire";
import type { Simulator } from "../../simulator";
import { BaseService } from "../base-service";
import { InvalidWireConnectionError } from "../../errors";
import type { Position } from "@digital-logic-sim/shared-types";
import { round2 } from "../../utils";

export class BlueprintService extends BaseService {
	constructor(sim: Simulator) {
		super(sim);

		this.init();
	}

	private init(): void {
		this.sim.on("sim.save-chip.start", ({ chipName }) =>
			this.saveBlueprint(chipName),
		);

		this.sim.on("sim.import-blueprint.start", ({ blueprintString }) =>
			this.loadBlueprint(blueprintString),
		);
	}

	public loadBlueprint(blueprintString: string): void {
		// TODO: add validation
		this.sim.chipLibraryService.register(JSON.parse(blueprintString));
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

	private serializeChip(chip: Chip): ChipBlueprint {
		const renderState = chip.getRenderState();

		return {
			id: chip.id,
			spec: {
				chipType: chip.spec.chipType,
				name: chip.spec.name,
			},
			renderState: {
				color: renderState.color,
				position: this.normalizePosition(renderState.position),
			},
		};
	}

	private serializeWire(wire: Wire): WireBlueprint {
		const renderState = wire.getRenderState();

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
			renderState: {
				color: renderState.color,
				controlPoints: renderState.controlPoints.map((controlPoint) =>
					this.normalizePosition(controlPoint),
				),
			},
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

			mappings[externalPinLabel] = this.getExternalIOPort(chip);
		}

		return { inputMappings, outputMappings };
	}

	private getExternalIOPort(ioChip: IOChip): ExternalIOPort {
		const ioPin = ioChip.getPin();

		const connectedWires =
			ioChip.ioChipType === "input"
				? this.sim.wireManager.getOutgoingWires(ioPin.id)
				: this.sim.wireManager.getIncomingWires(ioPin.id);

		const mappings = connectedWires.map((wire) => {
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

		return {
			position: this.normalizePosition(ioChip.getRenderState().position),
			mappings,
		};
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

	private normalizePosition(position: Position): Position {
		return {
			x: round2(position.x),
			y: round2(position.y),
		};
	}
}
