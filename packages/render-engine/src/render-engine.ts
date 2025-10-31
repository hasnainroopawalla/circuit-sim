export class RenderEngine {
	private device!: GPUDevice;

	private readonly gpuCanvasContext: GPUCanvasContext;

	constructor(args: { gpuCanvasContext: GPUCanvasContext }) {
		this.gpuCanvasContext = args.gpuCanvasContext;
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
}
