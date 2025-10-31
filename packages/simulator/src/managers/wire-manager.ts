import { Chip } from "../entities/chip";
import { Pin, PinType } from "../entities/pin";
import { Wire } from "../entities/wire";
import type { Simulator } from "../simulator";
import { BaseManager } from "./base-manager";

export class WireManager extends BaseManager {
	public readonly wires: Wire[];

	constructor(sim: Simulator) {
		super(sim);

		this.wires = [];

		this.init();
	}

	public init(): void {}

	public propagateWires(): void {
		// targetPin.nextValue = sourcePin.nextValue
		this.wires.forEach((wire) => {
			console.log("WIRE", wire.id, wire.startPinId, wire.endPinId);
			const startPin = this.getPin(wire.startPinId);
			const endPin = this.getPin(wire.endPinId);

			if (!startPin || !endPin) {
				return;
			}

			console.log("wire", wire.id, startPin, startPin);
			endPin.nextValue = startPin.nextValue;
		});
	}

	private getPin(pinId: string): Pin | undefined {
		const [chipId, pinType, chipPinId] = pinId.split(".");
		const chip = this.sim.chipManager.getChipById(chipId);
		return chip?.getPin(pinType as PinType, Number(chipPinId));
	}

	public spawnWire(startPinId: string, endPinId: string): void {
		const wire = new Wire({
			startPinId,
			endPinId,
		});

		this.wires.push(wire);
	}

	public removeWire(): void {}
}
