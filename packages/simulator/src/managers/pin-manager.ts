import type { Pin } from "../entities/pin";
import { entityIdService } from "../entity-id-service";
import type { Simulator } from "../simulator";
import { BaseManager } from "./base-manager";

export class PinManager extends BaseManager {
	constructor(sim: Simulator) {
		super(sim);
	}

	public spawnPin(pin: Pin, chipId: string): void {
		const pinId = entityIdService.generatePinId(
			chipId,
			pin.pinIdx,
			pin.pinType,
		);

		pin.setId(pinId);
	}
}
