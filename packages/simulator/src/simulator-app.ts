import { RenderEngine } from "@digital-logic-sim/render-engine";
import { Simulator } from "./simulator";
import { Clock } from "./clock";
import { Camera } from "./camera";
import { MeshUtils } from "./mesh-utils";
import { ScenarioLoader } from "./scenarios";
import type { Entity } from "./entities/entity";

// managers
import { LayoutManager } from "./layouts/layout-manager";
import { OverlayManager } from "./managers/overlay-manager";
import { InputManager } from "./managers/input-manager";

// services
import { MousePositionService } from "./services/mouse-position-service";
import { SettingsService } from "./services/settings-service";

type SimulatorAppArgs = { canvas: HTMLCanvasElement };

export class SimulatorApp {
	public sim: Simulator;
	public overlayManager: OverlayManager;
	public settingsService: SettingsService;

	private clock: Clock;

	private renderEngine: RenderEngine;
	private renderEngineInitPromise: Promise<void>;

	private layoutManager: LayoutManager;
	private inputManager: InputManager;

	private mousePositionService: MousePositionService;

	private camera: Camera;

	private animationId: number | null = null;

	// biome-ignore lint/correctness/noUnusedPrivateClassMembers: <temporary component>
	private scenarioLoader: ScenarioLoader;

	constructor(args: SimulatorAppArgs) {
		this.clock = new Clock({ showFrameTime: false });

		this.initializeCanvas(args.canvas);

		this.inputManager = new InputManager({ canvas: args.canvas });

		this.sim = new Simulator();

		this.settingsService = new SettingsService(this.sim);

		const gpuContext = args.canvas.getContext("webgpu");
		if (!gpuContext) {
			throw new Error("Failed to get WebGPU context");
		}

		this.renderEngine = new RenderEngine({
			gpuCanvasContext: gpuContext,
			settingsCtx: { get: () => this.settingsService.get() },
		});

		this.camera = new Camera({ canvas: args.canvas });

		this.mousePositionService = new MousePositionService({
			sim: this.sim,
			camera: this.camera,
			inputManager: this.inputManager,
		});

		this.layoutManager = new LayoutManager({
			sim: this.sim,
			camera: this.camera,
			mousePositionService: this.mousePositionService,
		});

		this.overlayManager = new OverlayManager({
			sim: this.sim,
			camera: this.camera,
			canvas: args.canvas,
		});

		this.renderEngineInitPromise = this.renderEngine.initialize().then(() => {
			this.registerCanvasResizeObserver(args.canvas);
		});

		this.scenarioLoader = new ScenarioLoader(this.sim);

		this.init();

		setTimeout(() => this.scenarioLoader.load("OrUsingNand"), 1000);
	}

	public async start(): Promise<void> {
		if (!this.animationId) {
			await this.renderEngineInitPromise;
			this.loop();
		}
	}

	public stop(): void {
		if (this.animationId) {
			cancelAnimationFrame(this.animationId);
			this.animationId = null;
		}

		this.inputManager.destroy();
	}

	private loop(): void {
		const deltaTime = this.clock.tick();

		this.sim.update();
		this.layoutManager.transitionState();

		this.inputManager.update(deltaTime);

		this.camera.update(deltaTime);

		const hoveredEntity = MeshUtils.getHoveredChipEntity(
			this.mousePositionService.getMousePosition().world,
			this.sim.chipManager.getBoardChips(),
		);

		this.overlayManager.update(hoveredEntity);

		this.layoutManager.update(hoveredEntity);

		this.updateCursor(hoveredEntity);

		this.renderEngine.render(
			this.layoutManager.getRenderables(),
			this.camera.getProjectionData(),
		);

		this.animationId = requestAnimationFrame(() => this.loop());
	}

	private initializeCanvas(canvas: HTMLCanvasElement): void {
		canvas.width = canvas.clientWidth * window.devicePixelRatio;
		canvas.height = canvas.clientHeight * window.devicePixelRatio;
		canvas.focus();
	}

	private registerCanvasResizeObserver(canvas: HTMLCanvasElement): void {
		const observer = new ResizeObserver((entries) => this.onResize(entries));
		observer.observe(canvas);
	}

	private init(): void {
		this.sim.on("sim.reset", () => this.reset());
		this.sim.on("sim.save-chip.finish", () => this.reset());

		this.sim.on("chip.delete.finish", ({ chipId }) =>
			this.overlayManager.deleteLabel(chipId),
		);

		// input manager subscriptions
		this.inputManager.onMouseScrollEvent((event) => {
			this.layoutManager.onMouseScrollEvent(event);
		});

		this.inputManager.onMouseButtonEvent((event, nature) => {
			this.layoutManager.onMouseButtonEvent(
				event,
				nature,
				this.mousePositionService.getMousePosition(),
			);
		});

		this.inputManager.onMouseMoveEvent(() => {
			this.layoutManager.onMouseMoveEvent(
				this.mousePositionService.getMousePosition(),
			);
		});

		this.inputManager.onKeyboardEvent((event, nature) => {
			this.layoutManager.onKeyboardEvent(event, nature);
		});
	}

	private async onResize(entries: ResizeObserverEntry[]): Promise<void> {
		entries.forEach(async (entry) => {
			const width = entry.contentBoxSize[0].inlineSize;
			const height = entry.contentBoxSize[0].blockSize;
			await this.renderEngine.onResize(width, height);
			this.camera.onResize(width, height);
		});
	}

	private reset(): void {
		this.sim.chipManager.reset();
		this.sim.wireManager.reset();
		this.overlayManager.reset();
	}

	private updateCursor(hoveredEntity: Entity | null): void {
		if (hoveredEntity) {
			document.body.style.cursor = "pointer";
		} else {
			document.body.style.cursor = "default";
		}
	}
}
