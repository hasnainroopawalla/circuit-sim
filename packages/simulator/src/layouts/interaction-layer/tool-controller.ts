import type { Tool } from "./tools";

export class ToolController {
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

	public render(): void {
		this.activeTool?.render();
	}

	public onPointerMove(event: PointerEvent): void {
		console.log(event.clientX, event.clientY);
		this.activeTool?.onPointerMove(event);
	}
}
