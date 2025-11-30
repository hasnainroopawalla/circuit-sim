import { RenderEngine } from "@digital-logic-sim/render-engine";
import { Simulator } from "./simulator";
import { LayoutManager } from "./layouts/layout-manager";
import { Camera } from "./layouts/simulation-layer";
import { Clock } from "./clock";
import { InputManager } from "./input-manager";

export class SimulatorApp {
	public sim: Simulator;

	private clock: Clock;

	private renderEngine: RenderEngine;
	private renderEngineInitPromise: Promise<void>;

	private layoutManager: LayoutManager;
	private inputManager: InputManager;

	private camera: Camera;

	private animationId: number | null = null;

	constructor(args: { canvas: HTMLCanvasElement }) {
		this.clock = new Clock({ showFrameTime: false });

		this.initializeCanvas(args.canvas);

		this.inputManager = new InputManager({ canvas: args.canvas });

		this.renderEngine = new RenderEngine({
			gpuCanvasContext: args.canvas.getContext("webgpu"),
		});

		this.sim = new Simulator();

		this.camera = new Camera({ canvas: args.canvas });

		this.layoutManager = new LayoutManager({
			sim: this.sim,
			camera: this.camera,
			screenHeight: args.canvas.height,
			screenWidth: args.canvas.width,
		});

		this.renderEngineInitPromise = this.renderEngine.initialize();

		this.registerInputManagerSubscriptions();
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
	}

	private loop(): void {
		const deltaTime = this.clock.tick();

		this.sim.update();

		this.inputManager.update(deltaTime);
		this.camera.update(deltaTime);

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

	private registerInputManagerSubscriptions(): void {
		this.inputManager.onMouseScrollEvent("scrollUp", (event) =>
			this.layoutManager.onMouseScrollEvent(event),
		);

		this.inputManager.onMouseScrollEvent("scrollDown", (event) =>
			this.layoutManager.onMouseScrollEvent(event),
		);

		this.inputManager.onMouseButtonEvent(
			"leftMouseButton",
			"click",
			(event, nature) => {
				const screenSpaceMousePosition = this.inputManager.getMousePosition();

				this.layoutManager.onMouseButtonEvent(event, nature, {
					screen: screenSpaceMousePosition,
					world: this.camera.getMouseWorldPosition(screenSpaceMousePosition),
				});
			},
		);
	}
}
