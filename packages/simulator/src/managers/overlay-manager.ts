import type { Camera } from "../camera";
import type { Simulator } from "../simulator";
import { BaseManager } from "./base-manager";

type OverlayManagerArgs = {
	camera: Camera;
	sim: Simulator;
};

export class OverlayManager extends BaseManager {
	private elements: Map<string, HTMLElement>;

	private camera: Camera;

	constructor(args: OverlayManagerArgs) {
		super(args.sim);

		this.elements = new Map();
		this.camera = args.camera;
	}

	public registerLabel(entityId: string, element: HTMLElement): void {
		this.elements.set(entityId, element);
	}

	public unregisterLabel(entityId: string): void {
		this.elements.delete(entityId);
	}

	public update(): void {
		for (const chip of this.sim.chipManager.chips) {
			const element = this.elements.get(chip.id);

			if (!element) {
				continue;
			}

			const screenSpacePosition = this.camera.computeScreenSpacePosition(
				chip.renderState.position,
			);

			element.style.left = `${screenSpacePosition.x}px`;
			element.style.top = `${screenSpacePosition.y}px`;
		}
	}
}
