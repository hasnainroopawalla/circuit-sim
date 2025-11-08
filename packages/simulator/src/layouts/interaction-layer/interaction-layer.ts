import { ToolManager } from "./tool-manager";
import { BaseLayer, type BaseLayerArgs } from "../base-layer";
import type { Renderable } from "@digital-logic-sim/render-engine";

export class InteractionLayer extends BaseLayer {
	private readonly toolManager: ToolManager;

	constructor(args: BaseLayerArgs) {
		super(args);
		this.toolManager = new ToolManager(args);
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

	public onPointerMove(event: PointerEvent): void {
		this.toolManager.onPointerMove(event);
	}
}
