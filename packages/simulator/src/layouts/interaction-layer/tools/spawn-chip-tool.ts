import type { Renderable } from "@digital-logic-sim/render-engine";
import { Tool, type ToolArgs } from "./tool";
import type { ChipSpec } from "../../../entities/chips";
import type { ButtonEvent, MouseButtonType } from "../../../input-manager";
import type { MousePosition } from "../../../types";

type SpawnChipToolArgs = ToolArgs & {
	chipSpec: ChipSpec;
};

export class SpawnChipTool extends Tool {
	private readonly chipSpec: ChipSpec;

	// TODO: ghost chip not required for right-click based spawn
	private readonly ghostChip: Renderable | null = null;

	constructor(args: SpawnChipToolArgs) {
		super(args);

		this.chipSpec = args.chipSpec;
	}

	public getRenderables(): Renderable[] {
		if (!this.ghostChip) {
			return [];
		}

		return [this.ghostChip];
	}

	public onMouseButtonEvent(
		event: MouseButtonType,
		nature: ButtonEvent,
		mousePosition: MousePosition,
	): void {
		this.sim.chipManager.spawnChip(
			this.chipSpec,
			{
				color: { r: 0, g: 1, b: 1, a: 1 },
				position: mousePosition.world,
			} /* renderSpec */,
		);
		this.deactivate();
	}

	public onPointerMove(event: PointerEvent): void {
		// TODO: update ghost chip position
	}
}
