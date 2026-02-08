import {
	type Chip,
	type CompositeChip,
	ChipType,
	IOChipType,
} from "../../entities/chips";
import type { RuntimePinMapping } from "../../entities/chips/composite-chip";
import type { Pin } from "../../entities/pin";
import type { WireConnection } from "../../entities/wire";
import { PinNotFoundError } from "../../errors";
import type { CompositeDefinition } from "../../services/blueprint-service";
import type {
	ChipDefinition,
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
		const compositeDefinition = compositeChip.spec.definition;

		const internalChipMap: InternalChipMap = new Map();

		this.spawnInternalChips(compositeChip, internalChipMap);

		const runtimeInputMappings = this.setupIOPinConnections(
			IOChipType.Input,
			compositeChip,
			compositeDefinition,
			internalChipMap,
		);

		const runtimeOutputMappings = this.setupIOPinConnections(
			IOChipType.Output,
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
		compositeChip.spec.definition.chips.forEach((chipBlueprint) => {
			const chipFactory =
				chipBlueprint.spec.chipType === ChipType.Composite
					? this.chipLibraryService.getChipFactory({
							kind: ChipType.Composite,
							name: chipBlueprint.spec.name,
						})
					: this.chipLibraryService.getChipFactory({
							kind: chipBlueprint.spec.chipType,
							name: chipBlueprint.spec.name,
						} as ChipDefinition);

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
			ioChipType === IOChipType.Input
				? compositeDefinition.inputMappings
				: compositeDefinition.outputMappings;

		const ioChipFactory = this.chipLibraryService.getChipFactory({
			kind: ChipType.IO,
			name: ioChipType,
		});

		Object.entries(chipMappings).forEach(
			([_externalPinLabel, externalIOPort]) => {
				const ioChip = this.chipManager.spawnChip(
					ioChipFactory,
					{
						position: externalIOPort.position,
					},
					{ parentCompositeId: compositeChip.id },
				);

				externalIOPort.mappings.forEach((mapping) => {
					const internalChipPin = this.getInternalChipPin(
						mapping.internalChipId,
						mapping.internalPinName,
						internalChipMap,
					);

					if (!internalChipPin) {
						throw new PinNotFoundError(
							mapping.internalChipId,
							"",
							mapping.internalPinName,
						);
					}

					const wireConnection: WireConnection =
						ioChipType === IOChipType.Input
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
							controlPoints: [],
						},
						{ parentCompositeId: compositeChip.id },
					);

					runtimeMappings.push({
						internalChip: ioChip,
						internalPin: internalChipPin,
					} as RuntimePinMapping<TIOChipType>);
				});
			},
		);

		return runtimeMappings;
	}

	private spawnInternalWires(
		compositeChip: Pick<CompositeChip, "id">,
		compositeDefinition: CompositeDefinition,
		internalChipMap: InternalChipMap,
	): void {
		compositeDefinition.wires.forEach((wire) => {
			const startPin = this.getInternalChipPin(
				wire.spec.start.chipId,
				wire.spec.start.pinName,
				internalChipMap,
			);

			if (!startPin) {
				throw new PinNotFoundError(
					wire.spec.start.chipId,
					"", // TODO: get the chip name
					wire.spec.start.pinName,
				);
			}

			const endPin = this.getInternalChipPin(
				wire.spec.end.chipId,
				wire.spec.end.pinName,
				internalChipMap,
			);

			if (!endPin) {
				throw new PinNotFoundError(
					wire.spec.end.chipId,
					"",
					wire.spec.end.pinName,
				);
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
}
