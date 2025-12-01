import type { Simulator } from "../simulator";
import { BaseService } from "./base-service";

export class BlueprintService extends BaseService {
	constructor(sim: Simulator) {
		super(sim);

		this.init();
	}

	private init(): void {
		this.sim.on("chip.save", () => this.saveChipAsBlueprint());
	}

	private saveChipAsBlueprint(): void {
		console.log("SAVING");
		this.sim.chipManager.chips.forEach((chip) => {
			if (chip.spec.type !== "io") {
				return;
			}
			console.log(
				"CHIP:",
				chip.spec.name,
				chip.spec.inputPins,
				chip.spec.outputPins,
			);
		});
	}
}
