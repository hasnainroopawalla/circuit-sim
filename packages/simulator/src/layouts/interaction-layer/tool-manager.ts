import type { Renderable } from "@digital-logic-sim/render-engine";
import type { Tool } from "./tools";

export class ToolManager {
	private activeTool: Tool | null;

	constructor() {
		this.activeTool = null;
	}

	public setActiveTool(tool: Tool): void {
		this.activeTool = tool;
	}

	public getActiveTool(): Tool | null {
		return this.activeTool;
	}

	public getRenderables(): Renderable[] {
		if (!this.activeTool) {
			return [];
		}
		return this.activeTool.getRenderables();
	}

	public onPointerDown(event: PointerEvent): void {
		this.activeTool?.onPointerDown(event);
	}
}
