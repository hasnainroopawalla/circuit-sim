import {
	EventingService,
	type IEvents,
	type Unsubscribe,
} from "./services/eventing-service";
import { ChipManager } from "./managers/chip-manager";
import { ChipLibraryService } from "./services/chip-library-service";
import { WireManager } from "./managers/wire-manager";
import { BlueprintService } from "./services/blueprint-service";

export class Simulator {
	// services
	public eventingService: EventingService;
	public chipLibraryService: ChipLibraryService;
	public blueprintService: BlueprintService;

	// managers
	public chipManager: ChipManager;
	public wireManager: WireManager;

	constructor() {
		// services
		this.eventingService = new EventingService();
		this.chipLibraryService = new ChipLibraryService(this);
		this.blueprintService = new BlueprintService(this);

		// managers
		this.chipManager = new ChipManager(this);
		this.wireManager = new WireManager(this);
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

	// TODO: add maxIterations
	public update(): void {
		// console.log("---- EXECUTE CALLED ----");
		let shouldRunLoop = true;

		while (shouldRunLoop) {
			const chipsChanged = this.chipManager.executeChips();
			const wiresChanged = this.wireManager.propagateWires();
			const pinsChanged = this.chipManager.commitAllPinValues();

			shouldRunLoop = chipsChanged || wiresChanged || pinsChanged;
		}
		// console.log("FINAL ->");
		// this.chipManager.chips.forEach((chip) => {
		// 	[...chip.inputPins].forEach((inputPin) => {
		// 		console.log(
		// 			chip.spec.name,
		// 			"In",
		// 			inputPin.currentValue,
		// 			inputPin.nextValue,
		// 		);
		// 	});

		// 	[...chip.outputPins].forEach((outputPin) => {
		// 		console.log(
		// 			chip.spec.name,
		// 			"Out",
		// 			outputPin.currentValue,
		// 			outputPin.nextValue,
		// 		);
		// 	});
		// });
	}

	// TODO: remove
	public setupNandGate(): void {
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

		this.wireManager.spawnWire({ startPinId: "0.out.0", endPinId: "2.in.0" }); // input 0 to AND in 0
		this.wireManager.spawnWire({ startPinId: "1.out.0", endPinId: "2.in.1" }); // input 1 to AND in 1
		// this.wireManager.spawnWire({startPinId: "2.out.0",endPinId: "3.in.0"}); // AND out to output

		this.wireManager.spawnWire({ startPinId: "2.out.0", endPinId: "3.in.0" }); // AND out to NOT in

		this.wireManager.spawnWire({ startPinId: "3.out.0", endPinId: "4.in.0" }); // NOT out to output

		inputChip1.setValue(true);
		this.update();

		inputChip2.setValue(true);
		this.update();

		console.log("Output Chip:", outputChip.getPin("in", 0)?.currentValue);
	}
}
