import { ToolManager } from "./tool-manager";
import { BaseLayer } from "../base-layer";
import type { Simulator } from "../../simulator";
import type { Renderable } from "@digital-logic-sim/render-engine";

export class InteractionLayer extends BaseLayer {
	private readonly toolManager: ToolManager;

	constructor(args: {
		sim: Simulator;
	}) {
		super(args);
		this.toolManager = new ToolManager();
	}

	public getRenderables(): Renderable[] {
		const tool = this.toolManager.getActiveTool();
		if (!tool) {
			return [];
		}

		return tool.getRenderables();
	}

	public onPointerDown(event: PointerEvent): void {
		this.toolManager.onPointerDown(event);
	}
}
