import type { Camera } from "../camera";
import type { Simulator } from "../simulator";
import { BaseManager } from "./base-manager";

type OverlayManagerArgs = {
	camera: Camera;
	sim: Simulator;
	canvas: HTMLCanvasElement;
};

export class OverlayManager extends BaseManager {
	private elements: Map<string, HTMLElement>;
	private canvas: HTMLCanvasElement;
	private camera: Camera;

	constructor(args: OverlayManagerArgs) {
		super(args.sim);

		this.elements = new Map();
		this.camera = args.camera;
		this.canvas = args.canvas;
	}

	public registerLabel(entityId: string, element: HTMLElement): void {
		this.elements.set(entityId, element);
	}

	public unregisterLabel(entityId: string): void {
		this.elements.delete(entityId);
	}

	public update(): void {
		for (const chip of this.sim.chipManager.getBoardChips()) {
			const element = this.elements.get(chip.id);

			if (!element) {
				continue;
			}

			const screenSpacePosition = this.camera.toScreenSpacePosition(
				chip.renderState.position,
			);

			const isVisible =
				screenSpacePosition.x >= 0 &&
				screenSpacePosition.x <= this.canvas.width &&
				screenSpacePosition.y >= 0 &&
				screenSpacePosition.y <= this.canvas.height;

			if (isVisible) {
				element.style.left = `${screenSpacePosition.x}px`;
				element.style.top = `${screenSpacePosition.y}px`;
				element.style.display = "block";
			} else {
				element.style.display = "none";
			}
		}
	}
}
