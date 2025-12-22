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
import {
	AtomicChipSpec,
	CompositeChipSpec,
	InputChipSpec,
	OutputChipSpec,
} from "./entities/chips";

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
		this.sim.blueprintService.importBlueprint(
			"NAND",
			`{"chips":[{"id":"2","spec":{"name":"AND","atomicChipType":"AND","chipType":"atomic","inputPins":[{"name":"in0"},{"name":"in1"}],"outputPins":[{"name":"out0"}]},"renderState":{"color":{"r":0,"g":0,"b":0,"a":1},"position":{"x":0.3,"y":1}},"inputPins":[{"id":"2.in.0","name":"in0"},{"id":"2.in.1","name":"in1"}],"outputPins":[{"id":"2.out.0","name":"out0"}]},{"id":"3","spec":{"name":"NOT","atomicChipType":"NOT","chipType":"atomic","inputPins":[{"name":"in0"}],"outputPins":[{"name":"out0"}]},"renderState":{"color":{"r":0,"g":0,"b":0,"a":1},"position":{"x":-1,"y":1}},"inputPins":[{"id":"3.in.0","name":"in0"}],"outputPins":[{"id":"3.out.0","name":"out0"}]}],"wires":[{"spec":{"start":{"chipId":"2","pinName":"out0"},"end":{"chipId":"3","pinName":"in0"}},"renderState":{"color":{"r":0,"g":0,"b":0,"a":1},"controlPoints":[]}}],"inputMappings":[{"externalPin":"IN0","internalChipId":"2","internalPinName":"in0"},{"externalPin":"IN1","internalChipId":"2","internalPinName":"in1"}],"outputMappings":[{"externalPin":"OUT4","internalChipId":"3","internalPinName":"out0"}]}`,
		);
		// const andChipSpec = this.sim.chipLibraryService.getChipSpecByName(
		// 	"AND",
		// ) as AtomicChipSpec;
		// const notChipSpec = this.sim.chipLibraryService.getChipSpecByName(
		// 	"NOT",
		// ) as AtomicChipSpec;
		// const inputChipSpec = this.sim.chipLibraryService.getChipSpecByName(
		// 	"INPUT",
		// ) as InputChipSpec;
		// const outputChipSpec = this.sim.chipLibraryService.getChipSpecByName(
		// 	"OUTPUT",
		// ) as OutputChipSpec;
		// // INPUT 0
		// const inputChip0 = this.sim.chipManager.spawnChip(inputChipSpec, {
		// 	color: { r: 0, g: 0, b: 0, a: 1 },
		// 	position: { x: 1.6, y: 1.3 },
		// });
		// // INPUT 1
		// const inputChip1 = this.sim.chipManager.spawnChip(inputChipSpec, {
		// 	color: { r: 0, g: 0, b: 0, a: 1 },
		// 	position: { x: 1.6, y: 0.8 },
		// });
		// // AND
		// const andChip = this.sim.chipManager.spawnChip(andChipSpec, {
		// 	color: { r: 0, g: 0, b: 0, a: 1 },
		// 	position: { x: 0.3, y: 1 },
		// });
		// // NOT
		// const notChip = this.sim.chipManager.spawnChip(notChipSpec, {
		// 	color: { r: 0, g: 0, b: 0, a: 1 },
		// 	position: { x: -1, y: 1 },
		// });
		// // OUTPUT 0
		// const outputChip = this.sim.chipManager.spawnChip(outputChipSpec, {
		// 	color: { r: 0, g: 0, b: 0, a: 1 },
		// 	position: { x: -2, y: 1 },
		// });
		// // input 0 to AND in 0
		// this.sim.wireManager.spawnWire(
		// 	{
		// 		startPin: inputChip0.getPin(),
		// 		endPin: andChip.getPin("in0")!,
		// 	},
		// 	{
		// 		color: { r: 0, g: 0, b: 0, a: 1 },
		// 		controlPoints: [],
		// 	},
		// );
		// // input 1 to AND in 1
		// this.sim.wireManager.spawnWire(
		// 	{
		// 		startPin: inputChip1.getPin(),
		// 		endPin: andChip.getPin("in1")!,
		// 	},
		// 	{
		// 		color: { r: 0, g: 0, b: 0, a: 1 },
		// 		controlPoints: [],
		// 	},
		// );
		// // AND out to NOT in
		// this.sim.wireManager.spawnWire(
		// 	{
		// 		startPin: andChip.getPin("out0")!,
		// 		endPin: notChip.getPin("in0")!,
		// 	},
		// 	{
		// 		color: { r: 0, g: 0, b: 0, a: 1 },
		// 		controlPoints: [],
		// 	},
		// );
		// // NOT out to output
		// this.sim.wireManager.spawnWire(
		// 	{
		// 		startPin: notChip.getPin("out0")!,
		// 		endPin: outputChip.getPin(),
		// 	},
		// 	{
		// 		color: { r: 0, g: 0, b: 0, a: 1 },
		// 		controlPoints: [],
		// 	},
		// );
		// this.sim.emit("chip.save", undefined);
		// const nandSpec = this.sim.chipLibraryService.getChipSpecByName(
		// 	"NAND",
		// ) as CompositeChipSpec;
		// this.sim.chipManager.spawnChip(nandSpec, {
		// 	color: { r: 0, g: 0, b: 0, a: 1 },
		// 	position: { x: 0.3, y: 0.1 },
		// });
	}
}
