import { BindGroupManager } from "./bind-group-manager";
import { BufferManager } from "./buffer-manager";
import { PipelineManager, PipelineType } from "./pipeline-manager";
import {
	type Renderable,
	type ChipRenderable,
	type WireRenderable,
	type CameraProjectionData,
	RenderableType,
} from "./render-engine.interface";
import { renderEngineConfig } from "./render-engine.config";
import { initWebGpu } from "./webgpu";

// shaders
import Shader from "./shaders/shader.wgsl?raw";
import NewLineShader from "./shaders/newLine.wgsl?raw";
import GridShader from "./shaders/gridShader.wgsl?raw";

// renderers
import { ChipRenderer } from "./render-engine.chip";
import { WireRenderer } from "./render-engine.wire";
import { GridRenderer } from "./render-engine.grid";

import type { Settings } from "@digital-logic-sim/shared-types";

export type SettingsContext = { get: () => Settings };

export class RenderEngine {
	private device!: GPUDevice;
	private bindGroupManager!: BindGroupManager;
	private bufferManager!: BufferManager;
	private pipelineManager!: PipelineManager;
	private depthTexture!: GPUTexture;
	private depthView!: GPUTextureView;
	private colorFormat!: GPUTextureFormat;

	private renderTargetView!: GPUTextureView;

	private gpuCanvasContext: GPUCanvasContext;

	// renderers
	private chipRenderer: ChipRenderer;
	private wireRenderer: WireRenderer;
	private gridRenderer: GridRenderer;

	private settingsCtx: SettingsContext;

	constructor(args: {
		gpuCanvasContext: GPUCanvasContext;
		settingsCtx: SettingsContext;
	}) {
		this.gpuCanvasContext = args.gpuCanvasContext;
		this.settingsCtx = args.settingsCtx;

		// renderers
		this.chipRenderer = new ChipRenderer(this);
		this.wireRenderer = new WireRenderer(this);
		this.gridRenderer = new GridRenderer(this);
	}

	public async initialize(): Promise<void> {
		if (!navigator.gpu) {
			throw new Error("WebGPU is not supported in this browser");
		}

		try {
			this.colorFormat = navigator.gpu.getPreferredCanvasFormat();
			this.device = await initWebGpu();
			this.gpuCanvasContext.configure({
				device: this.device,
				format: this.colorFormat,
				alphaMode: "premultiplied",
			});

			this.bindGroupManager = new BindGroupManager({ device: this.device });
			this.bufferManager = new BufferManager({ device: this.device });
			this.pipelineManager = new PipelineManager({ device: this.device });

			this.setupPipelines();
		} catch (err) {
			// biome-ignore lint/suspicious/noConsole: <Core error>
			console.log("RenderEngine init failed:", err);
		}
	}

	public get view() {
		return {
			device: this.device,
			bindGroupManager: this.bindGroupManager,
			bufferManager: this.bufferManager,
			pipelineManager: this.pipelineManager,
			depthTexture: this.depthTexture,
			depthView: this.depthView,
			renderTargetView: this.renderTargetView,
		};
	}

	public render(
		renderables: Renderable[],
		cameraProjectionData: CameraProjectionData,
	): void {
		const settings = this.settingsCtx.get();

		const { chips, wires } = this.partitionRenderables(renderables);
		this.uploadCamera(cameraProjectionData);

		const commandEncoder = this.device.createCommandEncoder();
		this.renderTargetView = this.gpuCanvasContext
			.getCurrentTexture()
			.createView();
		this.clearScreen(commandEncoder);

		// renderers

		if (settings.showGrid) {
			this.gridRenderer.render(commandEncoder);
		}

		this.chipRenderer.render(commandEncoder, chips);
		this.wireRenderer.render(commandEncoder, wires);

		this.device.queue.submit([commandEncoder.finish()]);
	}

	public async onResize(width: number, height: number): Promise<void> {
		await this.device.queue.onSubmittedWorkDone();

		this.gpuCanvasContext.canvas.width = width;
		this.gpuCanvasContext.canvas.height = height;
		this.gpuCanvasContext.unconfigure();
		this.gpuCanvasContext.configure({
			device: this.device,
			format: this.colorFormat,
			alphaMode: "premultiplied",
		});
		this.gpuCanvasContext.canvas.width = width;
		this.gpuCanvasContext.canvas.height = height;
		this.depthTexture.destroy();
		this.depthTexture = this.device.createTexture({
			size: [
				this.gpuCanvasContext.canvas.width,
				this.gpuCanvasContext.canvas.height,
			],
			format: "depth24plus",
			usage: GPUTextureUsage.RENDER_ATTACHMENT,
		});
		this.depthView = this.depthTexture.createView();
	}

	private uploadCamera(cameraProjectionData: CameraProjectionData): void {
		const cameraStaging = new Float32Array(
			2 * renderEngineConfig.matrixFloatSize,
		);

		cameraStaging.set(cameraProjectionData.viewProjectionMatrix, 0);
		cameraStaging.set(
			cameraProjectionData.viewProjectionInvMatrix,
			renderEngineConfig.matrixFloatSize,
		);

		this.device.queue.writeBuffer(
			this.bufferManager.cameraUBO,
			0,
			cameraStaging,
		);
	}

	private setupPipelines() {
		this.bindGroupManager.createBindGroupLayouts();

		this.bindGroupManager.createCameraBindGroup(
			this.bufferManager.createCameraBuffer(),
		);

		this.depthTexture = this.device.createTexture({
			size: [
				this.gpuCanvasContext.canvas.width,
				this.gpuCanvasContext.canvas.height,
			],
			format: "depth24plus",
			usage: GPUTextureUsage.RENDER_ATTACHMENT,
		});
		this.depthView = this.depthTexture.createView();

		const blendState: GPUBlendState = {
			color: { srcFactor: "one", dstFactor: "one-minus-src-alpha" },
			alpha: {
				srcFactor: "one",
				dstFactor: "one-minus-src-alpha",
			},
		};

		this.bufferManager.createVertexBuffer();
		this.bufferManager.createModelSBO(renderEngineConfig.radiusFloatSize);
		this.bufferManager.createModelSBO(0);

		this.bindGroupManager.createModelBindGroup(
			this.bufferManager.modelSBOs[0],
			renderEngineConfig.radiusFloatSize,
		); // TODO: hardcoded to 0?
		this.bindGroupManager.createModelBindGroup(
			this.bufferManager.modelSBOs[1],
			0,
		); // TODO: hardcoded to 1?

		this.pipelineManager.addPipeline({
			pipelineType: PipelineType.GenericShader,
			shader: Shader,
			bindGroupLayouts: [
				this.bindGroupManager.cameraBindGroupLayout,
				this.bindGroupManager.modelBindGroupLayout,
			],
			depthTesting: true,
			vertexLayout: undefined,
			blend: blendState,
			topology: "triangle-list",
		});

		this.pipelineManager.addPipeline({
			pipelineType: PipelineType.LineShader,
			shader: NewLineShader,
			bindGroupLayouts: [
				this.bindGroupManager.cameraBindGroupLayout,
				this.bindGroupManager.modelBindGroupLayout,
			],
			depthTesting: false,
			vertexLayout: undefined,
			blend: blendState,
			topology: "triangle-list",
		});

		this.pipelineManager.addPipeline({
			pipelineType: PipelineType.GridShader,
			shader: GridShader,
			bindGroupLayouts: [this.bindGroupManager.cameraBindGroupLayout],
			depthTesting: false,
			vertexLayout: undefined,
			blend: blendState,
			topology: "triangle-list",
		});
	}

	/**
	 * Splits the Renderables list based on the type.
	 */
	private partitionRenderables(renderables: Renderable[]): {
		chips: ChipRenderable[];
		wires: WireRenderable[];
	} {
		return renderables.reduce(
			(acc, renderable) => {
				switch (renderable.type) {
					case RenderableType.Chip:
						acc.chips.push(renderable);
						break;
					case RenderableType.Wire:
						acc.wires.push(renderable);
						break;
				}

				return acc;
			},
			{ chips: [] as ChipRenderable[], wires: [] as WireRenderable[] },
		);
	}

	private clearScreen(commandEncoder: GPUCommandEncoder): void {
		const passEncoder = commandEncoder.beginRenderPass({
			colorAttachments: [
				{
					view: this.renderTargetView,
					clearValue: { r: 0.25, g: 0.25, b: 0.25, a: 1.0 },
					loadOp: "clear",
					storeOp: "store",
				},
			],
			depthStencilAttachment: {
				view: this.depthView,
				depthClearValue: 1.0,
				depthLoadOp: "clear",
				depthStoreOp: "store",
			},
		});

		passEncoder.end();
	}
}
