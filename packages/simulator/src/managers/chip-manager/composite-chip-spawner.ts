import type { Chip, CompositeChip, IOChipType } from "../../entities/chips";
import type { RuntimePinMapping } from "../../entities/chips/composite-chip";
import type { Pin } from "../../entities/pin";
import type { WireConnection } from "../../entities/wire";
import {
	type Blueprint,
	type CompositeDefinition,
	BlueprintUtils,
} from "../../services/blueprint-service";
import type {
	ChipDefinition,
	ChipFactory,
	ChipLibraryService,
} from "../../services/chip-library-service";
import type { WireManager } from "../wire-manager";
import type { ChipManager } from "./chip-manager";

type InternalChipMap = Map<string /* chipId */, Chip>;

export class CompositeChipSpawner {
	private chipManager: ChipManager;
	private wireManager: WireManager;
	private chipLibraryService: ChipLibraryService;

	constructor(
		chipManager: ChipManager,
		wireManager: WireManager,
		chipLibraryService: ChipLibraryService,
	) {
		this.chipManager = chipManager;
		this.wireManager = wireManager;
		this.chipLibraryService = chipLibraryService;
	}

	public spawn(compositeChip: CompositeChip): void {
		const compositeDefinition = BlueprintUtils.getCompositeChipDefinition(
			compositeChip.spec.name,
			compositeChip.spec.blueprint,
		);

		const internalChipMap: InternalChipMap = new Map();

		this.spawnInternalChips(compositeChip, internalChipMap);

		const runtimeInputMappings = this.setupIOPinConnections(
			"input",
			compositeChip,
			compositeDefinition,
			internalChipMap,
		);

		const runtimeOutputMappings = this.setupIOPinConnections(
			"output",
			compositeChip,
			compositeDefinition,
			internalChipMap,
		);

		compositeChip.setMappings(runtimeInputMappings, runtimeOutputMappings);

		this.spawnInternalWires(
			compositeChip,
			compositeDefinition,
			internalChipMap,
		);
	}

	private spawnInternalChips(
		compositeChip: CompositeChip,
		internalChipMap: InternalChipMap,
	): void {
		const rootCompositeDefinition = BlueprintUtils.getCompositeChipDefinition(
			compositeChip.spec.name,
			compositeChip.spec.blueprint,
		);

		if (!rootCompositeDefinition) {
			throw new Error("Root composite definition does not exist");
		}

		rootCompositeDefinition.chips.forEach((chipBlueprint) => {
			let chipFactory: ChipFactory;
			if (chipBlueprint.spec.chipType === "composite") {
				const compositeDefinition = BlueprintUtils.getCompositeChipDefinition(
					chipBlueprint.spec.name,
					compositeChip.spec.blueprint,
				);

				if (!compositeDefinition) {
					throw new Error("Internal composite definition does not exist");
				}

				chipFactory = this.createCompositeFactory(
					chipBlueprint.spec.name,
					compositeDefinition,
					compositeChip.spec.blueprint,
				);
			} else {
				chipFactory = this.chipLibraryService.resolve({
					kind: chipBlueprint.spec.chipType,
					name: chipBlueprint.spec.name,
				} as ChipDefinition);
			}

			const internalChip = this.chipManager.spawnChip(
				chipFactory,
				chipBlueprint.renderState,
				{
					parentCompositeId: compositeChip.id,
				},
			);

			internalChipMap.set(chipBlueprint.id, internalChip);
		});
	}

	private setupIOPinConnections<TIOChipType extends IOChipType>(
		ioChipType: TIOChipType,
		compositeChip: Pick<CompositeChip, "id">, // TODO: Not needed
		compositeDefinition: CompositeDefinition,
		internalChipMap: InternalChipMap,
	): RuntimePinMapping<TIOChipType>[] {
		const runtimeMappings: RuntimePinMapping<TIOChipType>[] = [];

		const chipMappings =
			ioChipType === "input"
				? compositeDefinition.inputMappings
				: compositeDefinition.outputMappings;

		const ioChipFactory = this.chipLibraryService.resolve({
			kind: "io",
			name: ioChipType,
		});

		Object.entries(chipMappings).forEach(([_externalPinLabel, mappings]) => {
			const ioChip = this.chipManager.spawnChip(
				ioChipFactory,
				{
					color: { r: 1, g: 1, b: 1, a: 1 },
					position: { x: 10, y: 10 },
					// externalPinLabel,
				},
				{ parentCompositeId: compositeChip.id },
			);

			mappings.forEach((mapping) => {
				const internalChipPin = this.getInternalChipPin(
					mapping.internalChipId,
					mapping.internalPinName,
					internalChipMap,
				);

				if (!internalChipPin) {
					throw new Error(
						`Internal Chip Pin does not exist [chipId: ${mapping.internalChipId}][pinName: ${mapping.internalPinName}]`,
					);
				}

				const wireConnection: WireConnection =
					ioChipType === "input"
						? {
								startPin: ioChip.getPin(),
								endPin: internalChipPin,
							}
						: {
								startPin: internalChipPin,
								endPin: ioChip.getPin(),
							};

				this.wireManager.spawnWire(
					wireConnection,
					{
						color: { r: 1, g: 1, b: 1, a: 1 },
						controlPoints: [],
					},
					{ parentCompositeId: compositeChip.id },
				);

				runtimeMappings.push({
					internalChip: ioChip,
					internalPin: internalChipPin,
				} as RuntimePinMapping<TIOChipType>);
			});
		});

		return runtimeMappings;
	}

	private spawnInternalWires(
		compositeChip: Pick<CompositeChip, "id">, // TODO: full chip not needed
		compositeDefinition: CompositeDefinition,
		internalChipMap: InternalChipMap,
	): void {
		compositeDefinition.wires.forEach((wire) => {
			const startPin = this.getInternalChipPin(
				wire.spec.start.chipId,
				wire.spec.start.pinName,
				internalChipMap,
			);

			const endPin = this.getInternalChipPin(
				wire.spec.end.chipId,
				wire.spec.end.pinName,
				internalChipMap,
			);

			if (!startPin || !endPin) {
				throw new Error(`Pin does not exist: ${JSON.stringify(wire.spec)}`);
			}

			this.wireManager.spawnWire(
				{
					startPin,
					endPin,
				},
				wire.renderState,
				{ parentCompositeId: compositeChip.id },
			);
		});
	}

	private getInternalChipPin(
		chipId: string,
		pinName: string,
		internalChipMap: InternalChipMap,
	): Pin | undefined {
		const internalChip = internalChipMap.get(chipId);

		if (!internalChip) {
			return;
		}

		return internalChip.getPin(pinName);
	}

	private createCompositeFactory(
		compositeName: string,
		definition: CompositeDefinition,
		blueprint: Blueprint,
	): ChipFactory {
		const { inputPins, outputPins } = BlueprintUtils.getIOPinSpecs(definition);

		return {
			kind: "composite",
			spec: {
				chipType: "composite",
				name: compositeName,
				blueprint,
				inputPins,
				outputPins,
			},
		};
	}
}
