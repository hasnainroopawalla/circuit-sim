import type { Position } from "@digital-logic-sim/shared-types";
import type { Camera } from "../camera";
import type { Entity } from "../entities/entity";
import type { Simulator } from "../simulator";
import { BaseManager } from "./base-manager";

type OverlayManagerArgs = {
	camera: Camera;
	sim: Simulator;
	canvas: HTMLCanvasElement;
};

export type OverlayElementKind = "static" | "hover";

export class OverlayManager extends BaseManager {
	private staticElements: Map<string, HTMLElement>;
	private hoverElements: Map<string, HTMLElement>;

	private canvas: HTMLCanvasElement;
	private camera: Camera;

	constructor(args: OverlayManagerArgs) {
		super(args.sim);

		this.staticElements = new Map();
		this.hoverElements = new Map();

		this.camera = args.camera;
		this.canvas = args.canvas;
	}

	public registerLabel(
		entityId: string,
		element: HTMLElement,
		kind: OverlayElementKind,
	): void {
		switch (kind) {
			case "static":
				this.staticElements.set(entityId, element);
				break;
			case "hover":
				this.hoverElements.set(entityId, element);
				break;
		}
	}

	public unregisterLabel(entityId: string, kind: OverlayElementKind): void {
		switch (kind) {
			case "static":
				this.staticElements.delete(entityId);
				break;
			case "hover":
				this.hoverElements.delete(entityId);
				break;
		}
	}

	public update(hoveredEntity: Entity | null): void {
		this.updateStaticElements();

		this.clearHoverElements();

		if (hoveredEntity) {
			this.updateHoverElements(hoveredEntity);
		}
	}

	private updateHoverElements(hoveredEntity: Entity): void {
		if (hoveredEntity.entityType === "pin") {
			const screenSpacePosition = this.camera.toScreenSpacePosition(
				hoveredEntity.getPosition(),
			);

			const element = this.hoverElements.get(hoveredEntity.id);

			if (!element) {
				return;
			}

			screenSpacePosition.x =
				hoveredEntity.pinType === "in"
					? screenSpacePosition.x - 35
					: screenSpacePosition.x + 32;

			this.showElement(element, screenSpacePosition);
		}
	}

	private updateStaticElements(): void {
		for (const chip of this.sim.chipManager.getBoardChips()) {
			const element = this.staticElements.get(chip.id);

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
				this.showElement(element, screenSpacePosition);
			} else {
				this.hideElement(element);
			}
		}
	}

	private clearHoverElements(): void {
		this.hoverElements.forEach((element) => {
			this.hideElement(element);
		});
	}

	private showElement(
		element: HTMLElement,
		screenSpacePosition: Position,
	): void {
		element.style.left = `${screenSpacePosition.x}px`;
		element.style.top = `${screenSpacePosition.y}px`;
		element.style.display = "block";
	}

	private hideElement(element: HTMLElement): void {
		element.style.display = "none";
	}
}
