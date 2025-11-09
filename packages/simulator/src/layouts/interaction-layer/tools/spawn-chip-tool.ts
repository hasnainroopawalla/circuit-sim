import type { Renderable } from "@digital-logic-sim/render-engine";
import { Tool, type ToolArgs } from "./tool";
import type { ChipSpec } from "../../../entities/chips";

type SpawnChipToolArgs = ToolArgs & {
	chipSpec: ChipSpec;
};

export class SpawnChipTool extends Tool {
	private readonly chipSpec: ChipSpec;

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

	public onPointerDown(event: PointerEvent): void {
		this.sim.chipManager.spawnChip(
			this.chipSpec,
			{
				color: "#123456",
				position: {
					x: event.clientX,
					y: event.clientY,
				},
			} /* renderSpec */,
		);
		this.deactivate();
	}

	public onPointerMove(event: PointerEvent): void {
		// TODO: update ghost chip position
	}
}
