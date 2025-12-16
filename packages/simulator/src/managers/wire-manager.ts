import {
	Wire,
	type WireInitParams,
	type WireConnection,
} from "../entities/wire";
import { entityIdService } from "../entity-id-service";
import type { Simulator } from "../simulator";
import { didAnyChange } from "../utils";
import { BaseManager } from "./base-manager";

export class WireManager extends BaseManager {
	public readonly wires: Wire[];

	constructor(sim: Simulator) {
		super(sim);

		this.wires = [];
	}

	public propagateWires(): boolean {
		return didAnyChange(this.wires, (wire) => {
			if (wire.endPin.nextValue !== wire.startPin.nextValue) {
				wire.endPin.nextValue = wire.startPin.nextValue;
				return true;
			}

			return false;
		});
	}

	public spawnWire(
		wireConnection: WireConnection,
		wireInitParams: WireInitParams,
	): void {
		const wire = new Wire({ wireConnection, wireInitParams });

		const wireId = entityIdService.generateId();
		wire.setId(wireId);

		this.wires.push(wire);
	}
}
