import { RenderEngine, ToolController } from "@digital-logic-sim/render-engine";
import { Simulator } from "./simulator";

export class SimulatorApp {
	private readonly sim: Simulator;
	private readonly renderEngine: RenderEngine;
	private readonly toolController: ToolController;

	private animationId: number | null;
	private readonly renderEngineInitPromise: Promise<void>;

	constructor(args: { canvas: HTMLCanvasElement }) {
		const initializedCanvas = this.initializeCanvas(args.canvas);

		this.sim = new Simulator();
		this.toolController = new ToolController();
		this.renderEngine = new RenderEngine({
			sim: this.sim,
			toolController: this.toolController,
			gpuCanvasContext: initializedCanvas.getContext("webgpu"),
		});

		this.renderEngineInitPromise = this.renderEngine.initialize();

		this.animationId = null;
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

		this.renderEngine.render();

		this.animationId = requestAnimationFrame(() => this.loop());
	}

	private initializeCanvas(canvas: HTMLCanvasElement): HTMLCanvasElement {
		canvas.width = canvas.clientWidth * window.devicePixelRatio;
		canvas.height = canvas.clientHeight * window.devicePixelRatio;
		canvas.focus();
		return canvas;
	}
}
