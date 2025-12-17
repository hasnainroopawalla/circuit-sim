import { EntitySpawnOptions } from "../entities/chips";
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
	private wires: Wire[];

	constructor(sim: Simulator) {
		super(sim);

		this.wires = [];
	}

	/**
	 * Returns a list of chips that are currently on the board (not internal to composite chips).
	 */
	public getBoardWires(): Wire[] {
		return this.wires.filter((wire) => !wire.parentCompositeId);
	}

	public getInternalWires(parentCompositeId: string): Wire[] {
		return this.wires.filter(
			(wire) => wire.parentCompositeId === parentCompositeId,
		);
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
		opts?: EntitySpawnOptions,
	): void {
		const wire = new Wire({ wireConnection, wireInitParams, opts });

		const wireId = entityIdService.generateId();
		wire.setId(wireId);

		this.wires.push(wire);
	}
}
