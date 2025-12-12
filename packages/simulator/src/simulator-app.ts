import { RenderEngine } from "@digital-logic-sim/render-engine";
import { Simulator } from "./simulator";
import { Clock } from "./clock";
import { Camera } from "./camera";
import { MeshUtils } from "./mesh-utils";

// managers
import { LayoutManager } from "./layouts/layout-manager";
import { OverlayManager } from "./managers/overlay-manager";
import { InputManager } from "./managers/input-manager";

// services
import { MousePositionService } from "./services/mouse-position-service";

type SimulatorAppArgs = { canvas: HTMLCanvasElement };

export class SimulatorApp {
	public sim: Simulator;
	public overlayManager: OverlayManager;

	private clock: Clock;

	private renderEngine: RenderEngine;
	private renderEngineInitPromise: Promise<void>;

	private layoutManager: LayoutManager;
	private inputManager: InputManager;

	private mousePositionService: MousePositionService;

	private camera: Camera;

	private animationId: number | null = null;

	constructor(args: SimulatorAppArgs) {
		this.clock = new Clock({ showFrameTime: false });

		this.initializeCanvas(args.canvas);

		this.inputManager = new InputManager({ canvas: args.canvas });

		this.renderEngine = new RenderEngine({
			gpuCanvasContext: args.canvas.getContext("webgpu"),
		});

		this.sim = new Simulator();

		this.camera = new Camera({ canvas: args.canvas, sim: this.sim });

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

		this.registerInputManagerSubscriptions();
	}

	public async start(): Promise<void> {
		if (!this.animationId) {
			await this.renderEngineInitPromise;
			this.setupNandGate(); // TODO: remove
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

		this.inputManager.update(deltaTime);

		this.camera.update(deltaTime);

		this.overlayManager.update();

		this.layoutManager.hoveredEntity = MeshUtils.getHoveredChipEntity(
			this.mousePositionService.getMousePosition().world,
			this.sim.chipManager.chips,
		);

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

	private registerInputManagerSubscriptions(): void {
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

	public setupNandGate(): void {
		const andChipSpec = this.sim.chipLibraryService.getChipSpecByName("AND");
		const notChipSpec = this.sim.chipLibraryService.getChipSpecByName("NOT");

		const inputChipSpec =
			this.sim.chipLibraryService.getChipSpecByName("INPUT");
		const outputChipSpec =
			this.sim.chipLibraryService.getChipSpecByName("OUTPUT");

		if (!inputChipSpec || !outputChipSpec || !andChipSpec || !notChipSpec) {
			return;
		}

		// INPUT 0
		this.sim.chipManager.spawnChip(inputChipSpec, {
			color: { r: 1, g: 1, b: 1, a: 1 },
			position: { x: 1.6, y: 1.3 },
		});

		// INPUT 1
		this.sim.chipManager.spawnChip(inputChipSpec, {
			color: { r: 1, g: 1, b: 1, a: 1 },
			position: { x: 1.6, y: 0.8 },
		});

		// AND
		this.sim.chipManager.spawnChip(andChipSpec, {
			color: { r: 1, g: 1, b: 1, a: 1 },
			position: { x: 0.3, y: 1 },
		});

		// NOT
		this.sim.chipManager.spawnChip(notChipSpec, {
			color: { r: 1, g: 1, b: 1, a: 1 },
			position: { x: -1, y: 1 },
		});

		// OUTPUT 0
		this.sim.chipManager.spawnChip(outputChipSpec, {
			color: { r: 1, g: 1, b: 1, a: 1 },
			position: { x: -2, y: 1 },
		});

		// input 0 to AND in 0
		this.sim.wireManager.spawnWire(
			{
				startPinId: "0.out.0",
				endPinId: "2.in.0",
			},
			{
				color: { r: 1, g: 1, b: 1, a: 1 },
				controlPoints: [],
			},
		);

		// input 1 to AND in 1
		this.sim.wireManager.spawnWire(
			{
				startPinId: "1.out.0",
				endPinId: "2.in.1",
			},
			{
				color: { r: 1, g: 1, b: 1, a: 1 },
				controlPoints: [],
			},
		);

		// AND out to NOT in
		this.sim.wireManager.spawnWire(
			{
				startPinId: "2.out.0",
				endPinId: "3.in.0",
			},
			{
				color: { r: 1, g: 1, b: 1, a: 1 },
				controlPoints: [],
			},
		);

		// NOT out to output
		this.sim.wireManager.spawnWire(
			{
				startPinId: "3.out.0",
				endPinId: "4.in.0",
			},
			{
				color: { r: 1, g: 1, b: 1, a: 1 },
				controlPoints: [],
			},
		);

		this.sim.emit("chip.save", undefined);
	}
}
