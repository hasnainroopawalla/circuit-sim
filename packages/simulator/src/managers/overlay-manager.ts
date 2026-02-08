import type { Position, RectDimension } from "@digital-logic-sim/shared-types";
import type { Camera } from "../camera";
import type { Entity } from "../entities/entity";
import type { Simulator } from "../simulator";
import { BaseManager } from "./base-manager";
import { MeshUtils } from "../mesh-utils";
import { renderEngineConfig } from "@digital-logic-sim/render-engine";
import { EntityUtils } from "../entities/utils";
import { ChipLabelUtils, type ChipType } from "../entities/chips";
import type { PinType } from "../entities/pin";
import type { IChipSpawnFinishEvent } from "../services/eventing-service";

type OverlayManagerArgs = {
	camera: Camera;
	sim: Simulator;
	canvas: HTMLCanvasElement;
};

export type OverlayElementKind = "static" | "hover";

export type OverlayLabelData = {
	id: string;
	text: string;
	kind: OverlayElementKind;
	element?: HTMLElement; // attached after mount
} & (
	| {
			entityType: "chip";
	  }
	| { entityType: "pin"; pinType: PinType }
);

export class OverlayManager extends BaseManager {
	private labels: Map<string, OverlayLabelData>;

	private canvas: HTMLCanvasElement;
	private camera: Camera;

	constructor(args: OverlayManagerArgs) {
		super(args.sim);

		this.labels = new Map();

		this.camera = args.camera;
		this.canvas = args.canvas;

		this.init();
	}

	public attachRef(entityId: string, element: HTMLElement): void {
		const label = this.labels.get(entityId);

		if (label) {
			label.element = element;
		}
	}

	public unregisterLabel(labelId: string): void {
		this.labels.delete(labelId);
	}

	public update(hoveredEntity: Entity | null): void {
		this.updateStaticLabels();

		this.clearHoverElements();

		if (hoveredEntity) {
			this.updateHoverLabels(hoveredEntity);
		}
	}

	public reset(): void {
		this.labels = new Map();
		this.sim.emit("overlay.changed", undefined);
	}

	public getLabels(): OverlayLabelData[] {
		return Array.from(this.labels, ([_chipId, value]) => value);
	}

	private init(): void {
		this.sim.on("chip.spawn.finish", (data) => {
			this.onChipSpawnFinish(data);
		});
	}

	private updateHoverLabels(hoveredEntity: Entity): void {
		if (EntityUtils.isPin(hoveredEntity)) {
			const pinWorldPosition = hoveredEntity.getPosition();

			// add an extra offset between label and pin
			pinWorldPosition.x =
				hoveredEntity.pinType === "in"
					? pinWorldPosition.x + renderEngineConfig.pinSize + 0.1
					: pinWorldPosition.x - renderEngineConfig.pinSize - 0.08;

			const pinScreenSpacePosition =
				this.camera.toScreenSpacePosition(pinWorldPosition);

			const label = this.labels.get(hoveredEntity.id);

			if (!label || !label.element) {
				return;
			}

			this.showPinLabel(
				pinWorldPosition,
				pinScreenSpacePosition,
				label.element,
			);
		}
	}

	private updateStaticLabels(): void {
		for (const chip of this.sim.chipManager.getBoardChips()) {
			const label = this.labels.get(chip.id);

			if (!label || !label.element) {
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
					chip.spec.name,
					chipWorldPosition,
					chipScreenSpacePosition,
					chip.layout.labelDimensions,
					label.element,
				);
			} else {
				this.hideElement(label.element);
			}
		}
	}

	private clearHoverElements(): void {
		this.labels.forEach((label) => {
			if (label.element && label.kind === "hover") {
				this.hideElement(label.element);
			}
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
		name: string,
		worldPosition: Position,
		screenSpacePosition: Position,
		entityDimension: RectDimension,
		element: HTMLElement,
	): void {
		const worldSpaceBoundingBox = MeshUtils.getWorldSpaceBoundingBox(
			worldPosition,
			entityDimension,
		);

		const { width, height } = this.getDimensionsInScreenSpace(
			worldSpaceBoundingBox.minPosition,
			worldSpaceBoundingBox.maxPosition,
		);

		this.showElement(element, {
			numLines: ChipLabelUtils.splitChipName(name).length,
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
			height: number;
			numLines?: number;
			width?: number;
		},
	): void {
		element.style.display = "flex";

		element.style.left = `${props.position.x}px`;
		element.style.top = `${props.position.y}px`;

		if (props.width) {
			element.style.width = `${props.width}px`;
		}

		element.style.height = `${props.height}px`;

		const fontScale = !props.numLines
			? 0.7 // for pin labels
			: 1 / Math.max(1, props.numLines);

		element.style.fontSize = `${props.height * fontScale}px`;
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

	private onChipSpawnFinish({
		chipId,
		chipName,
		chipType,
		pins,
	}: IChipSpawnFinishEvent): void {
		if (!this.shouldRegisterChipLabel(chipType)) {
			return;
		}

		this.labels.set(chipId, {
			entityType: "chip" as const,
			kind: "static" as const,
			id: chipId,
			text: chipName,
		});

		pins.map((pin) =>
			this.labels.set(pin.id, {
				entityType: "pin" as const,
				kind: "hover" as const,
				id: pin.id,
				text: pin.name,
				pinType: pin.pinType,
			}),
		);

		this.sim.emit("overlay.changed", undefined);
	}

	private shouldRegisterChipLabel(chipType: ChipType): boolean {
		return chipType !== "io";
	}
}
