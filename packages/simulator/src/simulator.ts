import {
	EventingService,
	type IEvents,
	type Unsubscribe,
} from "./services/eventing-service";
import { EntityService } from "./services/entity-service";
import { ChipManager } from "./managers/chip-manager";
import { ChipLibraryService } from "./services/chip-library-service";
import { WireManager } from "./managers/wire-manager";
import { RenderEngine } from "@digital-logic-sim/render-engine";

export class Simulator {
	readonly renderEngine: RenderEngine;

	// services
	readonly eventingService: EventingService;
	readonly entityService: EntityService;
	readonly chipLibraryService: ChipLibraryService;

	// managers
	readonly chipManager: ChipManager;
	readonly wireManager: WireManager;

	constructor(args: { canvas: HTMLCanvasElement }) {
		this.renderEngine = new RenderEngine({
			gpuCanvasContext: args.canvas.getContext("webgpu"),
		});

		// services
		this.eventingService = new EventingService();
		this.entityService = new EntityService(this);
		this.chipLibraryService = new ChipLibraryService(this);

		// managers
		this.chipManager = new ChipManager(this);
		this.wireManager = new WireManager(this);

		this.setupNandGate();
	}

	public on<K extends keyof IEvents>(
		event: K,
		handler: (data: IEvents[K]) => void,
	): Unsubscribe {
		return this.eventingService.subscribe(event, handler);
	}

	public emit<K extends keyof IEvents>(event: K, data: IEvents[K]): void {
		this.eventingService.publish(event, data);
	}

	public async start(): Promise<void> {
		return this.renderEngine.initialize().then(() => this.loop());
	}

	private loop(): void {}

	// TODO: remove
	private setupNandGate(): void {
		const andChipSpec = this.chipLibraryService.getChipSpecByName("AND");
		const notChipSpec = this.chipLibraryService.getChipSpecByName("NOT");

		const inputChipSpec = this.chipLibraryService.getChipSpecByName("INPUT");
		const outputChipSpec = this.chipLibraryService.getChipSpecByName("OUTPUT");

		if (!inputChipSpec || !outputChipSpec || !andChipSpec || !notChipSpec) {
			return;
		}

		this.emit("chip.spawn", inputChipSpec);
		this.emit("chip.spawn", inputChipSpec);
		this.emit("chip.spawn", andChipSpec);
		this.emit("chip.spawn", notChipSpec);
		this.emit("chip.spawn", outputChipSpec);

		const inputChip = this.entityService.getEntityById("0");
		// const inputChip = this.entityService.getEntityById("0");
		const outputChip = this.entityService.getEntityById("2");
		const andChip = this.entityService.getEntityById("4");
		const notChip = this.entityService.getEntityById("8");

		if (!inputChip || !outputChip || !andChip || !notChip) {
			return;
		}

		this.wireManager.spawnWire("1", "5"); // input 0 to AND in 0
		this.wireManager.spawnWire("2", "6"); // input 1 to AND in 1

		this.wireManager.spawnWire("7", "9"); // AND out to NOT in

		this.wireManager.spawnWire("10", "12"); // NOT out to output

		console.log(this.entityService.entities);
	}
}
