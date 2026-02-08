import type { Renderable } from "@digital-logic-sim/render-engine";
import { SpawnChipTool, WiringTool, type Tool } from "./tools";
import type { Simulator } from "../../simulator";
import type {
	ButtonEvent,
	KeyboardButtonType,
	MouseButtonType,
} from "../../managers/input-manager";
import type { MousePosition } from "../../types";
import type { Entity } from "../../entities/entity";
import type { MousePositionService } from "../../services/mouse-position-service";
import type { Camera } from "../../camera";

type ToolManagerArgs = {
	sim: Simulator;
	camera: Camera;
	mousePositionService: MousePositionService;
};

export class ToolManager {
	private sim: Simulator;
	private camera: Camera;
	private mousePositionService: MousePositionService;

	private activeTool: Tool | null = null;

	constructor(args: ToolManagerArgs) {
		this.sim = args.sim;
		this.camera = args.camera;
		this.mousePositionService = args.mousePositionService;

		this.init();
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

	public onKeyboardEvent(
		event: KeyboardButtonType,
		nature: ButtonEvent,
	): boolean {
		return !!this.activeTool?.onKeyboardEvent(event, nature);
	}

	private setActiveTool<
		T extends Tool,
		Args extends {
			sim: Simulator;
			camera: Camera;
			deactivate: () => void;
			mousePositionService: MousePositionService;
		},
	>(
		Tool: new (args: Args) => T,
		args: Omit<Args, "sim" | "deactivate" | "mousePositionService" | "camera">,
	): void {
		if (this.activeTool) {
			return;
		}

		this.activeTool = new Tool({
			...args,
			mousePositionService: this.mousePositionService,
			sim: this.sim,
			camera: this.camera,
			deactivate: () => this.clearActiveTool(),
		} as Args);
	}

	private init(): void {
		this.sim.on("chip.spawn.start", ({ chipDefinition }) => {
			const chipFactory =
				this.sim.chipLibraryService.getChipFactory(chipDefinition);
			this.setActiveTool(SpawnChipTool, { chipFactory });
		});

		this.sim.on("wire.spawn.start", ({ startPin }) =>
			this.setActiveTool(WiringTool, { startPin }),
		);
	}

	private clearActiveTool(): void {
		this.activeTool = null;
	}
}
