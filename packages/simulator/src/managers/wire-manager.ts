import type { Pin, PinType } from "../entities/pin";
import { Wire, type WireRenderSpec, type WireSpec } from "../entities/wire";
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

	public spawnWire(wireSpec: WireSpec, renderSpec: WireRenderSpec): void {
		const startPin = this.getPin(wireSpec.startPinId);
		const endPin = this.getPin(wireSpec.endPinId);

		if (!startPin || !endPin) {
			throw new Error("SpawnWire error: start/end pin does not exist");
		}

		const wire = new Wire({ spec: wireSpec, renderSpec, startPin, endPin });

		this.wires.push(wire);
	}

	public removeWire(): void {}

	private getPin(pinId: string): Pin | undefined {
		const { chipId, pinType, chipPinId } = entityIdService.parsePinId(pinId);

		const chip = this.sim.chipManager.getChipById(chipId);
		return chip?.getPin(pinType as PinType, Number(chipPinId));
	}
}
