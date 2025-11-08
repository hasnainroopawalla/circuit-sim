import { RenderEngine } from "@digital-logic-sim/render-engine";
import { Simulator } from "./simulator";
import { LayoutManager } from "./layouts/layout-manager";

export class SimulatorApp {
	private readonly sim: Simulator;

	private readonly renderEngine: RenderEngine;
	private readonly renderEngineInitPromise: Promise<void>;

	private readonly layoutManager: LayoutManager;

	private animationId: number | null = null;

	constructor(args: { canvas: HTMLCanvasElement }) {
		this.initializeCanvas(args.canvas);

		this.renderEngine = new RenderEngine({
			gpuCanvasContext: args.canvas.getContext("webgpu"),
		});

		this.sim = new Simulator();

		this.layoutManager = new LayoutManager({
			sim: this.sim,
		});

		this.renderEngineInitPromise = this.renderEngine.initialize();

		this.registerPointerSubscriptions(args.canvas);

		this.sim.setupNandGate();
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

		this.renderEngine.render(this.layoutManager.getRenderables());

		this.animationId = requestAnimationFrame(() => this.loop());
	}

	private initializeCanvas(canvas: HTMLCanvasElement): void {
		canvas.width = canvas.clientWidth * window.devicePixelRatio;
		canvas.height = canvas.clientHeight * window.devicePixelRatio;
		canvas.focus();
	}

	private registerPointerSubscriptions(canvas: HTMLCanvasElement): void {
		canvas.addEventListener("pointerdown", (e) =>
			this.layoutManager.onPointerDown(e as PointerEvent),
		);

		canvas.addEventListener("pointermove", (e) =>
			this.layoutManager.onPointerMove(e as PointerEvent),
		);
	}
}
