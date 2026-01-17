import type {
	ChipRenderable,
	Renderable,
	PinRenderable,
} from "@digital-logic-sim/render-engine";
import { Tool, type ToolArgs } from "./tool";
import { GhostChip, type GhostChipSpec } from "../../../entities/chips";
import type {
	ButtonEvent,
	KeyboardButtonType,
	MouseButtonType,
} from "../../../managers/input-manager";
import type { MousePosition } from "../../../types";
import type { Entity } from "../../../entities/entity";
import type { PinType } from "../../../entities/pin";
import {
	ChipLibraryUtils,
	type ChipFactory,
} from "../../../services/chip-library-service";
import { BlueprintUtils } from "../../../services/blueprint-service";
import { COLORS } from "../../../services/color-service";

type SpawnChipToolArgs = ToolArgs & {
	chipFactory: ChipFactory;
};

export class SpawnChipTool extends Tool {
	private chipFactory: ChipFactory;

	private ghostChip: GhostChip;

	constructor(args: SpawnChipToolArgs) {
		super(args);

		this.chipFactory = args.chipFactory;

		this.ghostChip = new GhostChip(this.getPinCount(args.chipFactory), {
			position: args.mousePositionService.getMousePosition().world,
		});
	}

	public getRenderables(): Renderable[] {
		if (!this.ghostChip) {
			return [];
		}

		return [this.createGhostChipRenderable()];
	}

	public onMouseButtonEvent(
		event: MouseButtonType,
		nature: ButtonEvent,
		_mousePosition: MousePosition,
	): void {
		switch (event) {
			case "leftMouseButton": {
				switch (nature) {
					case "click": {
						this.handleLeftMouseButtonClick();
						break;
					}
				}
			}
		}
	}

	public onMouseMoveEvent(
		mousePosition: MousePosition,
		_hoveredEntity: Entity | null,
	): void {
		this.ghostChip.setPosition(mousePosition.world);
	}

	public onKeyboardEvent(
		event: KeyboardButtonType,
		_nature: ButtonEvent,
	): void {
		switch (event) {
			case "Escape":
				this.deactivate();
				break;
		}
	}

	private createGhostChipRenderable(): ChipRenderable {
		const createPinRenderable = (
			numPins: number,
			pinType: PinType,
		): PinRenderable[] =>
			Array.from({ length: numPins }, (_, pinIdx) => ({
				type: "pin",
				value: false,
				position: this.ghostChip.layout.getPinPosition(pinIdx, pinType),
				color: COLORS.Ghost,
			}));

		return {
			type: "chip",
			color: this.ghostChip.renderState.color,
			position: this.ghostChip.renderState.position,
			dimensions: this.ghostChip.layout.dimensions,
			label: "",
			inputPins: createPinRenderable(this.ghostChip.spec.numInputPins, "in"),
			outputPins: createPinRenderable(this.ghostChip.spec.numOutputPins, "out"),
		};
	}

	private handleLeftMouseButtonClick(): void {
		// TODO: Generate color
		// const chipColor = ColorService.generateChipColor();
		// const chipColor = { r: 0.97, g: 0.35, b: 0.7, a: 1.0 };

		this.sim.chipManager.spawnChip(
			this.chipFactory,
			{
				// color: chipColor,
				position: this.ghostChip.renderState.position,
			} /* init params */,
		);

		this.deactivate();
	}

	private getPinCount(chipFactory: ChipFactory): GhostChipSpec {
		const chipSpec = ChipLibraryUtils.getChipSpec(chipFactory);

		const { inputPins, outputPins } =
			chipSpec.chipType === "composite"
				? BlueprintUtils.getIOPinSpecs(chipSpec.definition)
				: chipSpec;

		return {
			numInputPins: inputPins.length,
			numOutputPins: outputPins.length,
		};
	}
}
