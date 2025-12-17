import type { Chip, CompositeChip, IOChipType } from "../../entities/chips";
import type { RuntimePinMapping } from "../../entities/chips/composite-chip";
import type { Pin } from "../../entities/pin";
import type { WireConnection } from "../../entities/wire";
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
		const internalChipMap: InternalChipMap = new Map();

		this.spawnInternalChips(compositeChip, internalChipMap);

		const runtimeInputMappings = this.setupIOPinConnections(
			"input",
			compositeChip,
			internalChipMap,
		);

		const runtimeOutputMappings = this.setupIOPinConnections(
			"output",
			compositeChip,
			internalChipMap,
		);

		compositeChip.setMappings(runtimeInputMappings, runtimeOutputMappings);

		this.spawnInternalWires(compositeChip, internalChipMap);
	}

	private spawnInternalChips(
		compositeChip: CompositeChip,
		internalChipMap: InternalChipMap,
	): void {
		compositeChip.spec.blueprint.chips.forEach((chipBlueprint) => {
			const chipFactory = this.chipLibraryService.resolve({
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
		compositeChip: CompositeChip,
		internalChipMap: InternalChipMap,
	): RuntimePinMapping<TIOChipType>[] {
		const runtimeMappings: RuntimePinMapping<TIOChipType>[] = [];

		const chipMappings =
			ioChipType === "input"
				? compositeChip.spec.blueprint.inputMappings
				: compositeChip.spec.blueprint.outputMappings;

		const ioChipFactory = this.chipLibraryService.resolve({
			kind: "io",
			name: ioChipType,
		});

		chipMappings.forEach((mapping) => {
			const ioChip = this.chipManager.spawnChip(
				ioChipFactory,
				{
					color: { r: 1, g: 1, b: 1, a: 1 },
					position: { x: 10, y: 10 },
					externalPinName: mapping.externalPin,
				},
				{ parentCompositeId: compositeChip.id },
			);

			const internalChipPin = this.getInternalChipPin(
				mapping.internalChipId,
				mapping.internalPinName,
				internalChipMap,
			);

			if (!internalChipPin) {
				throw new Error("Internal Chip Pin does not exist");
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

			// spawn wires
			this.wireManager.spawnWire(wireConnection, {
				color: { r: 1, g: 1, b: 1, a: 1 },
				controlPoints: [],
			});

			runtimeMappings.push({
				internalChip: ioChip,
				internalPin: internalChipPin,
			} as RuntimePinMapping<TIOChipType>);
		});

		return runtimeMappings;
	}

	private spawnInternalWires(
		compositeChip: CompositeChip,
		internalChipMap: InternalChipMap,
	): void {
		compositeChip.spec.blueprint.wires.forEach((wire) => {
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
