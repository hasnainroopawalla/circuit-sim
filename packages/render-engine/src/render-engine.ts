import { BindGroupManager } from "./bind-group-manager";
import { BufferManager } from "./buffer-manager";
import { PipelineManager, PipelineType } from "./pipeline-manager";
import type { CameraEntity, Renderable } from "./render-engine.interface";
import { mat4 } from "wgpu-matrix";

// shaders
import Shader from "./shaders/shader.wgsl?raw";
import LineShader from "./shaders/line.wgsl?raw";
import { renderEngineConfig } from "./render-engine.config";

export class RenderEngine {
	private device!: GPUDevice;
	private bindGroupManager!: BindGroupManager;
	private bufferManager!: BufferManager;
	private pipelineManager!: PipelineManager;
	private depthTexture!: GPUTexture;

	private gpuCanvasContext: GPUCanvasContext;

	constructor(args: {
		gpuCanvasContext: GPUCanvasContext;
	}) {
		this.gpuCanvasContext = args.gpuCanvasContext;
	}

	public async initialize(): Promise<void> {
		if (!navigator.gpu) {
			throw new Error("WebGPU is not supported in this browser");
		}

		return navigator.gpu.requestAdapter().then(async (adapter) => {
			if (!adapter) {
				throw new Error("No appropriate GPUAdapter found");
			}

			return adapter.requestDevice().then((device) => {
				this.device = device;
				console.log("Device created!");

				this.gpuCanvasContext.configure({
					device,
					format: "bgra8unorm",
					alphaMode: "premultiplied",
				});

				this.bindGroupManager = new BindGroupManager({ device });
				this.bufferManager = new BufferManager({ device });
				this.pipelineManager = new PipelineManager({ device });

				this.setupPipelines();
			});
		});
	}

	public render(renderables: Renderable[], camera: CameraEntity): void {
		this.uploadCamera(camera.eye);
	}

	private uploadCamera(cameraEye: Float32Array): void {
		const cameraStaging = new Float32Array(
			2 * renderEngineConfig.matrixFloatSize,
		);
		const viewProjMatrix = this.getViewProjectionMatrix(cameraEye);

		cameraStaging.set(viewProjMatrix, 0);
		cameraStaging.set(
			mat4.inverse(viewProjMatrix),
			renderEngineConfig.matrixFloatSize,
		);

		this.device.queue.writeBuffer(
			this.bufferManager.cameraUBO,
			0,
			cameraStaging,
		);
	}

	// TODO @abhishek: rename method
	private setupPipelines() {
		this.bindGroupManager.createBindGroupLayouts();

		this.bindGroupManager.createCameraBindGroup(
			this.bufferManager.createCameraBuffer(),
		);

		// this.bindGroupManager.createBackgroundBindGroup(
		// 	this.bufferManager.createBackgroundBuffer(renderEngineConfig.mapSize),
		// );

		//this.bufferManager.createComputeBuffers(renderEngineConfig.mapSize);

		//this.bindGroupManager.createComputeBindGroup(
		//	this.bufferManager.computeBuffers,
		//	this.bufferManager.computeUBO,
		//	renderEngineConfig.mapSize,
		//);

		//this.bindGroupManager.createPatchBindGroup(
		//	this.bufferManager.createPatchBuffer(),
		//);

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

		const blendState: GPUBlendState = {
			color: { srcFactor: "one", dstFactor: "one-minus-src-alpha" },
			alpha: {
				srcFactor: "one",
				dstFactor: "one-minus-src-alpha",
			},
		};

		this.bufferManager.createVertexBuffer();
		this.bufferManager.createModelSBO();

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
			shader: LineShader,
			bindGroupLayouts: [this.bindGroupManager.cameraBindGroupLayout],
			depthTesting: true,
			vertexLayout: vertexLayout,
			blend: blendState,
			topology: "line-list",
		});

		// this.pipelineManager.addPipeline({
		// 	pipelineType: PipelineType.GridShader,
		// 	shader: GridShader,
		// 	bindGroupLayouts: [this.bindGroupManager.cameraBindGroupLayout],
		// 	depthTesting: false,
		// 	blend: blendState,
		// 	topology: "triangle-list",
		// });

		// this.pipelineManager.addPipeline({
		// 	pipelineType: PipelineType.BackgroundShader,
		// 	shader: BackgroundShader,
		// 	bindGroupLayouts: [
		// 		this.bindGroupManager.cameraBindGroupLayout,
		// 		this.bindGroupManager.backgroundBindGroupLayout,
		// 	],
		// 	depthTesting: false,
		// 	topology: "triangle-list",
		// });

		//this.pipelineManager.addPipeline({
		//	pipelineType: PipelineType.MapEditorShader,
		//	shader: MapEditorShader,
		//	bindGroupLayouts: [this.bindGroupManager.backgroundBindGroupLayout],
		//	depthTesting: false,
		//});

		//this.pipelineManager.addComputePipeline(
		//	ComputeShader,
		//	this.bindGroupManager.computeBindGroupLayout,
		//);

		//this.pipelineManager.addPipeline({
		//	pipelineType: PipelineType.PatchShader,
		//	shader: PatchShader,
		//	bindGroupLayouts: [
		//		this.bindGroupManager.cameraBindGroupLayout,
		//		this.bindGroupManager.patchBindGroupLayout,
		//	],
		//	depthTesting: true,
		//	vertexLayout: undefined,
		//	blend: blendState,
		//});
	}

	private getViewProjectionMatrix(cameraEye: Float32Array): Float32Array {
		const { height: screenHeight, width: screenWidth } =
			this.gpuCanvasContext.canvas;

		const cameraTarget = mat4.add(
			cameraEye,
			[0, 0, 1] /* only look long z-axis */,
		);
		const camMatrix = mat4.lookAt(
			cameraEye,
			cameraTarget,
			renderEngineConfig.cameraUp,
		);
		const viewMatrix = mat4.inverse(camMatrix);

		const projectMatrix = mat4.perspective(
			(renderEngineConfig.cameraFOV * Math.PI) / 180,
			screenWidth / screenHeight,
			0.1,
			100,
		);

		return mat4.multiply(projectMatrix, viewMatrix);
	}

	private uploadRenderData(renderables: Renderable[]): void {}
}
