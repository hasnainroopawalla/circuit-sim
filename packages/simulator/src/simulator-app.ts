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

		this.registerSubscriptions();
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

		const hoveredEntity = MeshUtils.getHoveredChipEntity(
			this.mousePositionService.getMousePosition().world,
			this.sim.chipManager.getBoardChips(),
		);

		this.overlayManager.update(hoveredEntity);

		this.layoutManager.update(hoveredEntity);

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

	private registerSubscriptions(): void {
		this.sim.on("sim.reset", () => this.reset());

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
		this.overlayManager.reset();
		this.sim.resetService.resetSimulator();
	}

	public setupNandGate(): void {
		const spawnNand = () => {
			const andChipSpec = this.sim.chipLibraryService.resolve({
				kind: "atomic",
				name: "AND",
			});

			const notChipSpec = this.sim.chipLibraryService.resolve({
				kind: "atomic",
				name: "NOT",
			});

			const inputChipSpec = this.sim.chipLibraryService.resolve({
				kind: "io",
				name: "input",
			});

			const outputChipSpec = this.sim.chipLibraryService.resolve({
				kind: "io",
				name: "output",
			});

			// INPUT 0
			const inputChip0 = this.sim.chipManager.spawnChip(inputChipSpec, {
				color: { r: 0, g: 0, b: 0, a: 1 },
				position: { x: 1.6, y: 1.3 },
			});
			// INPUT 1
			const inputChip1 = this.sim.chipManager.spawnChip(inputChipSpec, {
				color: { r: 0, g: 0, b: 0, a: 1 },
				position: { x: 1.6, y: 0.8 },
			});
			// AND
			const andChip = this.sim.chipManager.spawnChip(andChipSpec, {
				color: { r: 0, g: 0, b: 0, a: 1 },
				position: { x: 0.3, y: 1 },
			});
			// NOT
			const notChip = this.sim.chipManager.spawnChip(notChipSpec, {
				color: { r: 0, g: 0, b: 0, a: 1 },
				position: { x: -1, y: 1 },
			});
			// OUTPUT 0
			const outputChip = this.sim.chipManager.spawnChip(outputChipSpec, {
				color: { r: 0, g: 0, b: 0, a: 1 },
				position: { x: -2, y: 1 },
			});
			// input 0 to AND in 0
			this.sim.wireManager.spawnWire(
				{
					startPin: inputChip0.getPin(),
					endPin: andChip.getPin("in0")!,
				},
				{
					color: { r: 0, g: 0, b: 0, a: 1 },
					controlPoints: [],
				},
			);
			// input 1 to AND in 1
			this.sim.wireManager.spawnWire(
				{
					startPin: inputChip1.getPin(),
					endPin: andChip.getPin("in1")!,
				},
				{
					color: { r: 0, g: 0, b: 0, a: 1 },
					controlPoints: [],
				},
			);
			// AND out to NOT in
			this.sim.wireManager.spawnWire(
				{
					startPin: andChip.getPin("out0")!,
					endPin: notChip.getPin("in0")!,
				},
				{
					color: { r: 0, g: 0, b: 0, a: 1 },
					controlPoints: [],
				},
			);
			// NOT out to output
			this.sim.wireManager.spawnWire(
				{
					startPin: notChip.getPin("out0")!,
					endPin: outputChip.getPin(),
				},
				{
					color: { r: 0, g: 0, b: 0, a: 1 },
					controlPoints: [],
				},
			);
		};

		spawnNand();

		// this.sim.blueprintService.loadBlueprint(compositeAndBlueprint);
	}
}
const compositeAndBlueprint = {
	root: "composite-AND",

	definitions: {
		"composite-AND": {
			chips: [
				{
					id: "5",
					spec: {
						chipType: "composite",
						name: "NAND",
					},
					renderState: {
						color: { r: 0, g: 0, b: 0, a: 1 },
						position: { x: 0.3, y: 1 },
					},
				},
				{
					id: "6",
					spec: {
						chipType: "atomic",
						name: "NOT",
					},
					renderState: {
						color: { r: 0, g: 0, b: 0, a: 1 },
						position: { x: -1, y: 1 },
					},
				},
			],
			wires: [
				{
					spec: {
						start: { chipId: "5", pinName: "OUT" },
						end: { chipId: "6", pinName: "in0" },
					},
					renderState: { color: { r: 0, g: 0, b: 0, a: 1 }, controlPoints: [] },
				},
			],
			inputMappings: {
				"C-AND A": [{ internalChipId: "5", internalPinName: "IN A" }],
				"C-AND B": [{ internalChipId: "5", internalPinName: "IN B" }],
			},
			outputMappings: {
				"C-AND OUT": [{ internalChipId: "6", internalPinName: "out0" }],
			},
		},

		NAND: {
			chips: [
				{
					id: "2",
					spec: {
						chipType: "atomic",
						name: "AND",
					},
					renderState: {
						color: { r: 0, g: 0, b: 0, a: 1 },
						position: { x: 0.3, y: 1 },
					},
				},
				{
					id: "3",
					spec: {
						chipType: "atomic",
						name: "NOT",
					},
					renderState: {
						color: { r: 0, g: 0, b: 0, a: 1 },
						position: { x: -1, y: 1 },
					},
				},
			],
			wires: [
				{
					spec: {
						start: { chipId: "2", pinName: "out0" },
						end: { chipId: "3", pinName: "in0" },
					},
					renderState: { color: { r: 0, g: 0, b: 0, a: 1 }, controlPoints: [] },
				},
			],
			inputMappings: {
				"IN A": [{ internalChipId: "2", internalPinName: "in0" }],
				"IN B": [{ internalChipId: "2", internalPinName: "in1" }],
			},
			outputMappings: {
				OUT: [{ internalChipId: "3", internalPinName: "out0" }],
			},
		},
	},
};
