import { BindGroupManager } from "./bind-group-manager";
import { BufferManager } from "./buffer-manager";
import { PipelineManager, PipelineType } from "./pipeline-manager";
import type {
	Renderable,
	ChipRenderable,
	WireRenderable,
	CameraProjectionData,
} from "./render-engine.interface";
import { renderEngineConfig } from "./render-engine.config";

// shaders
import Shader from "./shaders/shader.wgsl?raw";
import LineShader from "./shaders/line.wgsl?raw";
import NewLineShader from "./shaders/newLine.wgsl?raw";
import GridShader from "./shaders/gridShader.wgsl?raw";

// renderers
import { ChipRenderer } from "./render-engine.chip";
import { WireRenderer } from "./render-engine.wire";
import { GridRenderer } from "./render-engine.grid";

export class RenderEngine {
	private device!: GPUDevice;
	private bindGroupManager!: BindGroupManager;
	private bufferManager!: BufferManager;
	private pipelineManager!: PipelineManager;
	private depthTexture!: GPUTexture;
	private depthView!: GPUTextureView;

	private renderTargetView!: GPUTextureView;

	private gpuCanvasContext: GPUCanvasContext;

	// renderers
	private chipRenderer: ChipRenderer;
	private wireRenderer: WireRenderer;
	private gridRenderer: GridRenderer;

	constructor(args: {
		gpuCanvasContext: GPUCanvasContext;
	}) {
		this.gpuCanvasContext = args.gpuCanvasContext;

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
			const adapter = await navigator.gpu.requestAdapter();
			if (!adapter) {
				throw new Error("No appropriate GPUAdapter found");
			}

			this.device = await adapter.requestDevice();

			console.log("Device created!");

			this.gpuCanvasContext.configure({
				device: this.device,
				format: "bgra8unorm",
				alphaMode: "premultiplied",
			});

			this.bindGroupManager = new BindGroupManager({ device: this.device });
			this.bufferManager = new BufferManager({ device: this.device });
			this.pipelineManager = new PipelineManager({ device: this.device });

			this.setupPipelines();
		} catch (err) {
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
		const { chips, wires } = this.partitionRenderables(renderables);
		this.uploadCamera(cameraProjectionData);

		const commandEncoder = this.device.createCommandEncoder();
		this.renderTargetView = this.gpuCanvasContext
			.getCurrentTexture()
			.createView();
		this.clearScreen(commandEncoder);

		// renderers
		this.gridRenderer.render(commandEncoder);
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
			format: "bgra8unorm",
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

		const vertexLayout: GPUVertexBufferLayout[] = [
			{
				arrayStride: 2 * 4,
				attributes: [{ shaderLocation: 0, offset: 0, format: "float32x2" }],
			},
		];

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
		this.bufferManager.createModelSBO();
		this.bufferManager.createModelSBO();

		this.bindGroupManager.createModelBindGroup(this.bufferManager.modelSBOs[0]); // TODO: hardcoded to 0?
		this.bindGroupManager.createModelBindGroup(this.bufferManager.modelSBOs[1]); // TODO: hardcoded to 1?

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
				this.bindGroupManager.modelBindGroupLayout],
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
				renderable.type === "chip"
					? acc.chips.push(renderable)
					: acc.wires.push(renderable);
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
