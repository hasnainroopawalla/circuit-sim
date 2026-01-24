import type { Position, RectDimension } from "@digital-logic-sim/shared-types";
import type { Camera } from "../camera";
import type { Entity } from "../entities/entity";
import type { Simulator } from "../simulator";
import { BaseManager } from "./base-manager";
import { MeshUtils } from "../mesh-utils";
import { renderEngineConfig } from "@digital-logic-sim/render-engine";
import { EntityUtils } from "../entities/utils";

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

	public reset(): void {
		this.staticElements = new Map();
		this.hoverElements = new Map();

		this.sim.emit("overlay.reset", undefined);
	}

	private updateHoverElements(hoveredEntity: Entity): void {
		if (hoveredEntity.entityType === "pin") {
			const pinWorldPosition = hoveredEntity.getPosition();

			// add an extra offset between label and pin
			pinWorldPosition.x =
				hoveredEntity.pinType === "in"
					? pinWorldPosition.x + renderEngineConfig.pinSize + 0.1
					: pinWorldPosition.x - renderEngineConfig.pinSize - 0.08;

			const pinScreenSpacePosition =
				this.camera.toScreenSpacePosition(pinWorldPosition);

			const element = this.hoverElements.get(hoveredEntity.id);

			if (!element) {
				return;
			}

			this.showPinLabel(pinWorldPosition, pinScreenSpacePosition, element);
		}
	}

	private updateStaticElements(): void {
		for (const chip of this.sim.chipManager.getBoardChips()) {
			const element = this.staticElements.get(chip.id);

			if (!element) {
				continue;
			}

			const { position: chipWorldPosition } = chip.getRenderState();

			const chipScreenSpacePosition =
				this.camera.toScreenSpacePosition(chipWorldPosition);

			const isVisible =
				chipScreenSpacePosition.x >= 0 &&
				chipScreenSpacePosition.x <= this.canvas.width &&
				chipScreenSpacePosition.y >= 0 &&
				chipScreenSpacePosition.y <= this.canvas.height;

			if (isVisible) {
				this.showChipLabel(
					chipWorldPosition,
					chipScreenSpacePosition,
					chip.layout.dimensions,
					element,
				);
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

	private showPinLabel(
		worldPosition: Position,
		screenSpacePosition: Position,
		element: HTMLElement,
	) {
		const worldSpaceBoundingBox = MeshUtils.getWorldSpaceBoundingBox(
			worldPosition,
			{
				width: renderEngineConfig.pinSize,
				height: renderEngineConfig.pinSize,
			},
		);

		const { height } = this.getDimensionsInScreenSpace(
			worldSpaceBoundingBox.minPosition,
			worldSpaceBoundingBox.maxPosition,
		);

		this.showElement(element, {
			height,
			position: screenSpacePosition,
		});
	}

	private showChipLabel(
		worldPosition: Position,
		screenSpacePosition: Position,
		entityDimension: RectDimension,
		element: HTMLElement,
	): void {
		const worldSpaceBoundingBox = MeshUtils.getWorldSpaceBoundingBox(
			worldPosition,
			entityDimension,
		);

		const boundingBoxWithOffset = {
			minPosition: {
				x: worldSpaceBoundingBox.minPosition.x + renderEngineConfig.pinSize,
				y: worldSpaceBoundingBox.minPosition.y + renderEngineConfig.pinSize,
			},
			maxPosition: {
				x: worldSpaceBoundingBox.maxPosition.x - renderEngineConfig.pinSize,
				y: worldSpaceBoundingBox.maxPosition.y - renderEngineConfig.pinSize,
			},
		};

		const { width, height } = this.getDimensionsInScreenSpace(
			boundingBoxWithOffset.minPosition,
			boundingBoxWithOffset.maxPosition,
		);

		this.showElement(element, {
			width,
			height,
			position: screenSpacePosition,
		});
	}

	private hideElement(element: HTMLElement): void {
		element.style.display = "none";
	}

	private showElement(
		element: HTMLElement,
		props: {
			position: Position;
			width?: number;
			height?: number;
		},
	): void {
		element.style.display = "flex";

		element.style.left = `${props.position.x}px`;
		element.style.top = `${props.position.y}px`;

		if (props.width) {
			element.style.width = `${props.width}px`;
		}

		if (props.height) {
			element.style.height = `${props.height}px`;
			element.style.fontSize = `${props.height * 0.7}px`;
		}
	}

	private getDimensionsInScreenSpace(
		minPosition: Position,
		maxPosition: Position,
	): RectDimension {
		const { x: screenSpaceMinX, y: screenSpaceMinY } =
			this.camera.toScreenSpacePosition({
				x: minPosition.x,
				y: minPosition.y,
			});

		const { x: screenSpaceMaxX, y: screenSpaceMaxY } =
			this.camera.toScreenSpacePosition({
				x: maxPosition.x,
				y: maxPosition.y,
			});

		const width = Math.abs(screenSpaceMaxX - screenSpaceMinX);
		const height = Math.abs(screenSpaceMaxY - screenSpaceMinY);

		return { width, height };
	}
}
