import type { Pin, PinType } from "../entities/pin";
import { Wire, type WireSpec } from "../entities/wire";
import type { Simulator } from "../simulator";
import { didAnyChange } from "../utils";
import { BaseManager } from "./base-manager";

export class WireManager extends BaseManager {
	public readonly wires: Wire[];

	constructor(sim: Simulator) {
		super(sim);

		this.wires = [];

		this.init();
	}

	public init(): void {}

	public propagateWires(): boolean {
		return didAnyChange(this.wires, (wire) => {
			const startPin = this.getPin(wire.spec.startPinId);
			const endPin = this.getPin(wire.spec.endPinId);

			if (!startPin || !endPin) {
				return false;
			}

			if (endPin.nextValue !== startPin.nextValue) {
				endPin.nextValue = startPin.nextValue;
				return true;
			}

			return false;
		});
	}

	private getPin(pinId: string): Pin | undefined {
		const [chipId, pinType, chipPinId] = pinId.split(".");
		const chip = this.sim.chipManager.getChipById(chipId);
		return chip?.getPin(pinType as PinType, Number(chipPinId));
	}

	public spawnWire(wireSpec: WireSpec): void {
		const renderSpec = {
			color: "red",
		};

		const wire = new Wire(wireSpec, renderSpec);

		this.wires.push(wire);
	}

	public removeWire(): void {}
}
