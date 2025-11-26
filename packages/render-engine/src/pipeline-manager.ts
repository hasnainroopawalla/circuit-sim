export enum PipelineType {
	GenericShader,
	GridShader,
	BackgroundShader,
	LineShader,
}

type PipelineManagerProps = { device: GPUDevice };

export class PipelineManager {
	public computePipeline!: GPUComputePipeline;

	private pipelines: Map<PipelineType, GPURenderPipeline>;
	private device: GPUDevice;

	constructor(props: PipelineManagerProps) {
		this.device = props.device;
		this.pipelines = new Map();
	}

	public getPipeline(
		pipelineType: PipelineType,
	): GPURenderPipeline | undefined {
		return this.pipelines.get(pipelineType);
	}

	public addComputePipeline(
		shader: string,
		bindGroupLayout: GPUBindGroupLayout,
	): void {
		const pipelineLayout = this.device.createPipelineLayout({
			bindGroupLayouts: [bindGroupLayout],
		});

		this.computePipeline = this.device.createComputePipeline({
			layout: pipelineLayout,
			compute: {
				module: this.device.createShaderModule({
					code: shader,
				}),
				entryPoint: "main",
			},
		});
	}

	public addPipeline(args: {
		pipelineType: PipelineType;
		shader: string;
		bindGroupLayouts: GPUBindGroupLayout[];
		depthTesting?: boolean;
		vertexLayout?: GPUVertexBufferLayout[];
		blend?: GPUBlendState;
		topology: GPUPrimitiveTopology;
	}): void {
		const {
			pipelineType,
			shader,
			bindGroupLayouts,
			depthTesting,
			vertexLayout,
			blend,
			topology,
		} = args;
		const pipelineLayout = this.createPipelineLayout(
			this.device,
			bindGroupLayouts,
		);

		this.pipelines.set(
			pipelineType,
			this.device.createRenderPipeline({
				vertex: {
					module: this.device.createShaderModule({
						code: shader,
					}),
					entryPoint: "vs_main",
					buffers: vertexLayout,
				},

				fragment: {
					module: this.device.createShaderModule({
						code: shader,
					}),
					entryPoint: "fs_main",
					targets: [
						{
							format: navigator.gpu.getPreferredCanvasFormat(),
							blend,
						},
					],
				},

				primitive: {
					topology: topology,
				},

				layout: pipelineLayout,
				depthStencil: {
					depthWriteEnabled: depthTesting,
					depthCompare: "less",
					format: "depth24plus",
				},
			}),
		);
	}

	private createPipelineLayout(
		device: GPUDevice,
		bindGroupLayouts: GPUBindGroupLayout[],
	): GPUPipelineLayout {
		return device.createPipelineLayout({
			bindGroupLayouts,
		});
	}
}
