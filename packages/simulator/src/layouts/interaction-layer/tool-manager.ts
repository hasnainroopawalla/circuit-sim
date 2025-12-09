import type { Renderable } from "@digital-logic-sim/render-engine";
import { SpawnChipTool, WiringTool, type Tool } from "./tools";
import type { Simulator } from "../../simulator";
import type {
	ButtonEvent,
	MouseButtonType,
} from "../../managers/input-manager";
import type { MousePosition } from "../../types";
import type { Entity } from "../../entities/entity";

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

	public onMouseButtonEvent(
		event: MouseButtonType,
		nature: ButtonEvent,
		mousePosition: MousePosition,
		hoveredEntity: Entity | null,
	): boolean {
		// console.log("active tool", this.activeTool);
		return !!this.activeTool?.onMouseButtonEvent(
			event,
			nature,
			mousePosition,
			hoveredEntity,
		);
	}

	public onPointerMove(event: PointerEvent): boolean {
		return !!this.activeTool?.onPointerMove(event);
	}

	private setActiveTool<
		T extends Tool,
		Args extends { sim: Simulator; deactivate: () => void },
	>(Tool: new (args: Args) => T, args: Omit<Args, "sim" | "deactivate">): void {
		if (this.activeTool) {
			return;
		}

		this.activeTool = new Tool({
			...args,
			sim: this.sim,
			deactivate: () => this.clearActiveTool(),
		} as Args);
	}

	private registerSubscriptions(): void {
		this.sim.on("chip.spawn", (chipSpec) =>
			this.setActiveTool(SpawnChipTool, { chipSpec }),
		);

		this.sim.on("wire.spawn", ({ startPin }) =>
			this.setActiveTool(WiringTool, { startPin }),
		);
	}

	private clearActiveTool(): void {
		this.activeTool = null;
	}
}
