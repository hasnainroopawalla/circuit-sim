import {
	EventingService,
	type IEvents,
	type Unsubscribe,
} from "./services/eventing-service";
import { ChipManager } from "./managers/chip-manager";
import { ChipLibraryService } from "./services/chip-library-service";
import { WireManager } from "./managers/wire-manager";
import { RenderEngine } from "@digital-logic-sim/render-engine";

export class Simulator {
	readonly renderEngine: RenderEngine;

	// services
	readonly eventingService: EventingService;
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

	// TODO: add maxIterations
	private execute(): void {
		console.log("---- EXECUTE CALLED ----");
		let changed: boolean;

		do {
			console.log("LOOP");
			changed = false;

			changed ||= this.chipManager.executeChips();
			changed ||= this.wireManager.propagateWires();
			changed ||= this.chipManager.commitAllPinValues();
		} while (changed);

		console.log("FINAL ->");
		this.chipManager.chips.forEach((chip) => {
			[...chip.inputPins].forEach((inputPin) => {
				console.log(
					chip.spec.name,
					"In",
					inputPin.currentValue,
					inputPin.nextValue,
				);
			});

			[...chip.outputPins].forEach((outputPin) => {
				console.log(
					chip.spec.name,
					"Out",
					outputPin.currentValue,
					outputPin.nextValue,
				);
			});
		});
	}

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

		const inputChip1 = this.chipManager.getChipById("0");
		const inputChip2 = this.chipManager.getChipById("1");
		const andChip = this.chipManager.getChipById("2");
		const notChip = this.chipManager.getChipById("3");
		const outputChip = this.chipManager.getChipById("4");

		if (!inputChip1 || !inputChip2 || !outputChip || !andChip || !notChip) {
			return;
		}

		this.wireManager.spawnWire("0.out.0", "2.in.0"); // input 0 to AND in 0
		this.wireManager.spawnWire("1.out.0", "2.in.1"); // input 1 to AND in 1
		// this.wireManager.spawnWire("2.out.0", "3.in.0"); // AND out to output

		this.wireManager.spawnWire("2.out.0", "3.in.0"); // AND out to NOT in

		this.wireManager.spawnWire("3.out.0", "4.in.0"); // NOT out to output

		this.execute();

		inputChip1.setValue(true);
		this.execute();

		inputChip2.setValue(true);
		this.execute();
	}
}
