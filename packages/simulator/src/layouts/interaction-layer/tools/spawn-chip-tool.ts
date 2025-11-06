import type { Renderable } from "@digital-logic-sim/render-engine";
import { Tool, type ToolArgs } from "./tool";

export class SpawnChipTool extends Tool {
	private readonly ghostChip: Renderable | null;

	constructor(args: ToolArgs) {
		super(args);

		this.ghostChip = null;
	}

	public getRenderables(): Renderable[] {
		if (!this.ghostChip) {
			return [];
		}

		return [this.ghostChip];
	}

	public onPointerDown(event: PointerEvent): void {
		// TODO: spawn chip here
	}
}
