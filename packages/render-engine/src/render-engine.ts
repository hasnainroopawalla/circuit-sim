import { LayoutManager } from "./layouts/layout-manager";
import type { Simulator } from "@digital-logic-sim/simulator";
import type { ToolController } from "./tool-controller";

export class RenderEngine {
	private device!: GPUDevice;

	private layoutManager: LayoutManager;
	private toolController: ToolController;

	private sim: Simulator;
	private gpuCanvasContext: GPUCanvasContext;

	constructor(args: {
		sim: Simulator;
		toolController: ToolController;
		gpuCanvasContext: GPUCanvasContext;
	}) {
		this.sim = args.sim;
		this.toolController = args.toolController;
		this.gpuCanvasContext = args.gpuCanvasContext;

		this.layoutManager = new LayoutManager({
			toolController: this.toolController,
			sim: this.sim,
		});

		this.registerPointerSubscriptions();
	}

	public async initialize(): Promise<void> {
		if (!navigator.gpu) {
			throw new Error("WebGPU is not supported in this browser");
		}

		return navigator.gpu.requestAdapter().then((adapter) => {
			if (!adapter) {
				throw new Error("No appropriate GPUAdapter found");
			}

			adapter.requestDevice().then((device) => {
				this.device = device;
				console.log("Device created!");
			});
		});
	}

	public render(): void {
		this.layoutManager.renderAll();
	}

	private registerPointerSubscriptions(): void {
		this.gpuCanvasContext.canvas.addEventListener("pointermove", (e) =>
			this.toolController.onPointerMove(e as PointerEvent),
		);
	}
}
