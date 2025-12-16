import {
	EventingService,
	type IEvents,
	type Unsubscribe,
} from "./services/eventing-service";
import { ChipManager } from "./managers/chip-manager/chip-manager";
import { ChipLibraryService } from "./services/chip-library-service";
import { WireManager } from "./managers/wire-manager";
import { BlueprintService } from "./services/blueprint-service";
import { PinManager } from "./managers/pin-manager";

const simulatorConfig = {
	maxIterations: 1000,
};

export class Simulator {
	// services
	public eventingService: EventingService;
	public chipLibraryService: ChipLibraryService;
	public blueprintService: BlueprintService;

	// managers
	public chipManager: ChipManager;
	public pinManager: PinManager;
	public wireManager: WireManager;

	constructor() {
		// services
		this.eventingService = new EventingService();
		this.chipLibraryService = new ChipLibraryService(this);
		this.blueprintService = new BlueprintService(this);

		// managers
		this.wireManager = new WireManager(this);
		this.chipManager = new ChipManager(this);
		this.pinManager = new PinManager(this);
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

	public update(): void {
		const state = {
			shouldRunLoop: true,
			iterations: 0,
		};

		while (
			state.shouldRunLoop &&
			state.iterations < simulatorConfig.maxIterations
		) {
			state.iterations += 1;

			const chipsChanged = this.chipManager.executeChips();
			const wiresChanged = this.wireManager.propagateWires();
			const pinsChanged = this.chipManager.commitAllPinValues();

			state.shouldRunLoop = chipsChanged || wiresChanged || pinsChanged;
		}
	}
}
