import { RenderEngine } from "@digital-logic-sim/render-engine";
import { Simulator } from "./simulator";
import { LayoutManager } from "./layouts/layout-manager";
import { Camera } from "./layouts/simulation-layer";
import { Clock } from "./clock";
import { InputManager } from "./input-manager";
import { MeshUtils } from "./mesh-utils";
import type { MousePosition } from "./types";

type SimulatorAppArgs = { canvas: HTMLCanvasElement };

export class SimulatorApp {
	public sim: Simulator;

	private clock: Clock;

	private renderEngine: RenderEngine;
	private renderEngineInitPromise: Promise<void>;

	private layoutManager: LayoutManager;
	private inputManager: InputManager;

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

		this.layoutManager = new LayoutManager({
			sim: this.sim,
			camera: this.camera,
			screenHeight: args.canvas.height,
			screenWidth: args.canvas.width,
		});

		this.renderEngineInitPromise = this.renderEngine.initialize().then(() => {
			this.registerCanvasResizeObserver(args.canvas);
		});

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

		this.inputManager.destroy();
	}

	private loop(): void {
		const deltaTime = this.clock.tick();

		this.sim.update();

		this.inputManager.update(deltaTime);
		this.camera.update(deltaTime);

		this.layoutManager.hoveredEntity = MeshUtils.getHoveredEntity(
			this.getMousePosition().world,
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
				this.layoutManager.onMouseButtonEvent(
					event,
					nature,
					this.getMousePosition(),
				);
			},
		);

		this.inputManager.onMouseButtonEvent(
			"leftMouseButton",
			"press",
			(event, nature) => {
				this.layoutManager.onMouseButtonEvent(
					event,
					nature,
					this.getMousePosition(),
				);
			},
		);

		(
			[
				{ event: "w", nature: "press" },
				{ event: "a", nature: "press" },
				{ event: "s", nature: "press" },
				{ event: "d", nature: "press" },
			] as const
		).forEach(({ event, nature }) => {
			this.inputManager.onKeyboardEvent(event, nature, (event, nature) =>
				this.layoutManager.onKeyboardEvent(event, nature),
			);
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

	private getMousePosition(): MousePosition {
		const screenSpaceMousePosition = this.inputManager.getMousePosition();

		return {
			screen: screenSpaceMousePosition,
			world: this.camera.getMouseWorldPosition(screenSpaceMousePosition),
		};
	}
}
