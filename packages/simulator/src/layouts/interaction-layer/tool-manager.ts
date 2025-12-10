import type { Renderable } from "@digital-logic-sim/render-engine";
import { SpawnChipTool, WiringTool, type Tool } from "./tools";
import type { Simulator } from "../../simulator";
import type {
	ButtonEvent,
	MouseButtonType,
} from "../../managers/input-manager";
import type { MousePosition } from "../../types";
import type { Entity } from "../../entities/entity";
import type { MousePositionService } from "../../services/mouse-position-service";

export class ToolManager {
	private sim: Simulator;
	private mousePositionService: MousePositionService;

	private activeTool: Tool | null = null;

	constructor(args: {
		sim: Simulator;
		mousePositionService: MousePositionService;
	}) {
		this.sim = args.sim;
		this.mousePositionService = args.mousePositionService;

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
		return !!this.activeTool?.onMouseButtonEvent(
			event,
			nature,
			mousePosition,
			hoveredEntity,
		);
	}

	public onMouseMoveEvent(
		mousePosition: MousePosition,
		hoveredEntity: Entity | null,
	): boolean {
		return !!this.activeTool?.onMouseMoveEvent(mousePosition, hoveredEntity);
	}

	private setActiveTool<
		T extends Tool,
		Args extends {
			sim: Simulator;
			deactivate: () => void;
			mousePositionService: MousePositionService;
		},
	>(
		Tool: new (args: Args) => T,
		args: Omit<Args, "sim" | "deactivate" | "mousePositionService">,
	): void {
		if (this.activeTool) {
			return;
		}

		console.log(args);

		this.activeTool = new Tool({
			...args,
			mousePositionService: this.mousePositionService,
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
