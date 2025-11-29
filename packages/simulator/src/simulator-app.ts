import { RenderEngine } from "@digital-logic-sim/render-engine";
import { Simulator } from "./simulator";
import { LayoutManager } from "./layouts/layout-manager";

export class SimulatorApp {
	public sim: Simulator;

	private renderEngine: RenderEngine;
	private renderEngineInitPromise: Promise<void>;

	private layoutManager: LayoutManager;

	private animationId: number | null = null;

	constructor(args: { canvas: HTMLCanvasElement }) {
		this.initializeCanvas(args.canvas);

		this.renderEngine = new RenderEngine({
			gpuCanvasContext: args.canvas.getContext("webgpu"),
		});

		this.sim = new Simulator();

		this.layoutManager = new LayoutManager({
			sim: this.sim,
			screenHeight: args.canvas.height,
			screenWidth: args.canvas.width,
		});

		this.renderEngineInitPromise = this.renderEngine.initialize();

		this.registerPointerSubscriptions(args.canvas);

		// this.sim.setupNandGate();
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
		this.sim.update();

		this.renderEngine.render(
			this.layoutManager.getRenderables(),
			this.layoutManager.getCamera(),
		);

		this.animationId = requestAnimationFrame(() => this.loop());
	}

	private initializeCanvas(canvas: HTMLCanvasElement): void {
		canvas.width = canvas.clientWidth * window.devicePixelRatio;
		canvas.height = canvas.clientHeight * window.devicePixelRatio;
		canvas.focus();
	}

	private registerPointerSubscriptions(canvas: HTMLCanvasElement): void {
		canvas.addEventListener("pointerdown", (event) =>
			this.layoutManager.onPointerDown(event as PointerEvent),
		);

		canvas.addEventListener("pointermove", (event) =>
			this.layoutManager.onPointerMove(event as PointerEvent),
		);

		canvas.addEventListener("keydown", (event) =>
			this.layoutManager.onKeyDown(event),
		);

		// TODO: bypass layout manager
		canvas.addEventListener("resize", (event) => event);
	}
}
