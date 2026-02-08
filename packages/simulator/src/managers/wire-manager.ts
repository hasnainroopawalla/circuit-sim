import type { EntitySpawnOptions } from "../entities/chips";
import {
	Wire,
	type WireInitParams,
	type WireConnection,
} from "../entities/wire";
import { entityIdService } from "../entity-id-service";
import { InvalidWireConnectionError } from "../errors";
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

	public reset(): void {
		this.wires = [];
	}

	public spawnWire(
		wireConnection: WireConnection,
		wireInitParams: WireInitParams,
		opts?: EntitySpawnOptions,
	): void {
		if (!wireConnection.startPin || !wireConnection.endPin) {
			throw new InvalidWireConnectionError(undefined, undefined);
		}

		const wire = new Wire({
			wireConnection: this.normalizeWireDirection(wireConnection),
			wireInitParams,
			opts,
		});

		const wireId = entityIdService.generateId();
		wire.setId(wireId);

		this.wires.push(wire);
	}

	public getWire(startPinId: string, endPinId: string): Wire | undefined {
		return this.getBoardWires().find(
			(wire) => wire.startPin.id === startPinId && wire.endPin.id === endPinId,
		);
	}

	public getOutgoingWires(pinId: string): Wire[] {
		return this.getBoardWires().filter((wire) => wire.startPin.id === pinId);
	}

	public getIncomingWires(pinId: string): Wire[] {
		return this.getBoardWires().filter((wire) => wire.endPin.id === pinId);
	}

	private normalizeWireDirection(
		wireConnection: WireConnection,
	): WireConnection {
		const { startPin, endPin } = wireConnection;

		if (startPin.pinType === "out" && endPin.pinType === "in") {
			return wireConnection;
		}

		if (startPin.pinType === "in" && endPin.pinType === "out") {
			return {
				startPin: endPin,
				endPin: startPin,
			};
		}

		throw new InvalidWireConnectionError(startPin, endPin);
	}
}
