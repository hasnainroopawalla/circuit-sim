import type { Renderable } from "@digital-logic-sim/render-engine";
import { SpawnChipTool, type Tool } from "./tools";
import type { Simulator } from "../../simulator";

export class ToolManager {
	private readonly sim: Simulator;

	private activeTool: Tool | null = null;

	constructor(args: {
		sim: Simulator;
	}) {
		this.sim = args.sim;

		this.registerSubscriptions();
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

	public onPointerMove(event: PointerEvent): void {
		this.activeTool?.onPointerMove(event);
	}

	private setActiveTool<T extends Tool, Args extends { sim: Simulator }>(
		Tool: new (args: Args) => T,
		args: Omit<Args, "sim">,
	): void {
		this.activeTool = new Tool({ ...args, sim: this.sim } as Args);
	}

	private registerSubscriptions(): void {
		// TODO: should a new instance be created each time?
		this.sim.on("chip.spawn", (chipSpec) =>
			this.setActiveTool(SpawnChipTool, { chipSpec }),
		);
	}
}
