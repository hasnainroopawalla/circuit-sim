import type {
	ChipRenderable,
	Renderable,
	PinRenderable,
} from "@digital-logic-sim/render-engine";
import { Tool, type ToolArgs } from "./tool";
import { type ChipSpec, GhostChip } from "../../../entities/chips";
import type {
	ButtonEvent,
	MouseButtonType,
} from "../../../managers/input-manager";
import type { MousePosition } from "../../../types";
import type { Entity } from "../../../entities/entity";
import type { PinType } from "../../../entities/pin";

type SpawnChipToolArgs = ToolArgs & {
	chipSpec: ChipSpec;
};

export class SpawnChipTool extends Tool {
	private chipSpec: ChipSpec;

	private ghostChip: GhostChip;

	constructor(args: SpawnChipToolArgs) {
		super(args);

		this.chipSpec = args.chipSpec;
		this.ghostChip = new GhostChip(args.chipSpec, {
			color: { r: 0.71, g: 0.71, b: 0.71, a: 0.08 },
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
		mousePosition: MousePosition,
	): void {
		this.sim.chipManager.spawnChip(
			this.chipSpec,
			{
				color: { r: 0, g: 0, b: 0.5, a: 1 },
				position: mousePosition.world,
			} /* init params */,
		);
		this.deactivate();
	}

	public onMouseMoveEvent(
		mousePosition: MousePosition,
		_hoveredEntity: Entity | null,
	): void {
		console.log(mousePosition.world)
		this.ghostChip.setPosition(mousePosition.world);
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
				color: { r: 0.59, g: 0.59, b: 0.59, a: 0.7 },
			}));

		return {
			type: "chip",
			color: this.ghostChip.renderState.color,
			position: this.ghostChip.renderState.position,
			dimensions: this.ghostChip.layout.dimensions,
			label: this.chipSpec.name,
			inputPins: createPinRenderable(this.chipSpec.inputPins.length, "in"),
			outputPins: createPinRenderable(this.chipSpec.outputPins.length, "out"),
		};
	}
}
