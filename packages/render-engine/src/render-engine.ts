import { BindGroupManager } from "./bind-group-manager";
import { BufferManager } from "./buffer-manager";
import { PipelineManager, PipelineType } from "./pipeline-manager";
import type {
	CameraEntity,
	Renderable,
	ChipRenderable,
	WireRenderable,
} from "./render-engine.interface";
import { mat4, vec3, vec4 } from "wgpu-matrix";
import { renderEngineConfig } from "./render-engine.config";

// shaders
import Shader from "./shaders/shader.wgsl?raw";
import LineShader from "./shaders/line.wgsl?raw";

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

	public render(renderables: Renderable[], camera: CameraEntity): void {
		//const { chips, wires } = this.partitionRenderables(renderables);
		const chips: ChipRenderable[] = [
			//{
			//	dimensions: { width: 1, height: 1 },
			//	color: { r: 0, g: 1, b: 1, a: 1 },
			//	position: { x: 0, y: 0 },
			//	type: "chip",
			//	label: "stubChip",
			//},
		];
		const wires: WireRenderable[] = [
			{
				type: "wire",
				controlPoints: new Float32Array([-1, -1, 1, -1, 1, 1,-1,1,-1,-1]),
				color: { r: 0, g: 1, b: 1, a: 1 },
			},
		];

		this.uploadCamera(camera.eye);
		this.uploadChipRenderData(chips);
		const wireVertexCount = this.uploadWireRenderData(wires);

		const commandEncoder = this.device.createCommandEncoder();
		this.chipRenderPass(commandEncoder, chips.length /* chipCount */);
		this.wireRenderPass(commandEncoder, wireVertexCount);

		this.device.queue.submit([commandEncoder.finish()]);
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

		this.bindGroupManager.createModelBindGroup(this.bufferManager.modelSBOs[0]); // TODO: hardcoded to 0?

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

	private uploadChipRenderData(chipData: ChipRenderable[]): void {
		const modelMatrixData = new Float32Array(
			renderEngineConfig.chunkSize *
				(renderEngineConfig.matrixFloatSize +
					renderEngineConfig.colorFloatSize),
		);
		chipData.forEach((element, index) => {
			modelMatrixData.set(
				mat4.translate(
					mat4.scale(
						mat4.identity(),
						vec3.create(
							element.dimensions.width,
							element.dimensions.height,
							1.0,
						),
					),
					vec3.create(element.position.x, element.position.y),
				),
				index *
					(renderEngineConfig.matrixFloatSize +
						renderEngineConfig.colorFloatSize),
			);

			modelMatrixData.set(
				vec4.create(
					element.color.r,
					element.color.g,
					element.color.b,
					element.color.a,
				),
				index *
					(renderEngineConfig.matrixFloatSize +
						renderEngineConfig.colorFloatSize) +
					renderEngineConfig.matrixFloatSize,
			);
		});

		this.bufferManager.modelSBOs.forEach((modelSBO) => {
			this.device.queue.writeBuffer(
				modelSBO,
				0,
				modelMatrixData,
				0,
				chipData.length * renderEngineConfig.modelFloatSize,
			);
		});
	}

	private uploadWireRenderData(wireData: WireRenderable[]): number {
		const lineVertexData = new Float32Array(
			renderEngineConfig.chunkSize * renderEngineConfig.lineDataFloatSize,
		);
		let offset = 0;
		wireData.forEach((element) => {
			for (let i = 1; i < element.controlPoints.length / 2; ++i) {
				const start = element.controlPoints.subarray(2 * (i - 1), 2 * i);
				lineVertexData.set(start, offset + 4 * (i - 1));
				const end = element.controlPoints.subarray(2 * i, 2 * (i + 1));
				lineVertexData.set(end, offset + (4*i)-2);
			}
			offset += 2 * element.controlPoints.length - 4;
		});
		this.device.queue.writeBuffer(
			this.bufferManager.vertexBuffers[0],
			0 /* bufferOffset */,
			lineVertexData,
			0 /* dataOffset */,
			offset * Float32Array.BYTES_PER_ELEMENT,
		);
		return offset / 2; /* number of wire vertrices */
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

	private chipRenderPass(
		commandEncoder: GPUCommandEncoder,
		chipCount: number,
	): void {
		const passEncoder = commandEncoder.beginRenderPass({
			colorAttachments: [
				{
					view: this.gpuCanvasContext.getCurrentTexture().createView(),
					clearValue: { r: 0, g: 0, b: 0, a: 1.0 },
					loadOp: "clear",
					storeOp: "store",
				},
			],
			depthStencilAttachment: {
				view: this.depthTexture.createView(),
				depthClearValue: 1.0,
				depthLoadOp: "clear",
				depthStoreOp: "store",
			},
		});

		const pipeline = this.pipelineManager.getPipeline(
			PipelineType.GenericShader,
		);

		if (!pipeline) {
			throw new Error("GenericShader pipeline not initialized.");
		}

		passEncoder.setPipeline(pipeline);

		passEncoder.setBindGroup(0, this.bindGroupManager.cameraBindGroup);
		passEncoder.setBindGroup(1, this.bindGroupManager.modelBindGroups[0]);
		passEncoder.draw(6, chipCount, 0, 0);
		passEncoder.end();
	}

	private wireRenderPass(
		commandEncoder: GPUCommandEncoder,
		wireVertexCount: number,
	): void {
		const passEncoder = commandEncoder.beginRenderPass({
			colorAttachments: [
				{
					view: this.gpuCanvasContext.getCurrentTexture().createView(),
					clearValue: { r: 0, g: 0, b: 0, a: 1.0 },
					loadOp: "load",
					storeOp: "store",
				},
			],
			depthStencilAttachment: {
				view: this.depthTexture.createView(),
				depthClearValue: 1.0,
				depthLoadOp: "load",
				depthStoreOp: "store",
			},
		});

		const pipeline = this.pipelineManager.getPipeline(PipelineType.LineShader);

		if (!pipeline) {
			throw new Error("LineShader pipeline not initialized.");
		}

		passEncoder.setPipeline(pipeline);

		passEncoder.setBindGroup(0, this.bindGroupManager.cameraBindGroup);
		passEncoder.setVertexBuffer(0, this.bufferManager.vertexBuffers[0]);
		passEncoder.draw(wireVertexCount, 1, 0, 0);
		passEncoder.end();
	}
}
