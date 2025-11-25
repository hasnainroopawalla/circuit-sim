type BindGroupManagerProps = { device: GPUDevice };

export class BindGroupManager {
	public cameraBindGroup!: GPUBindGroup;

	public cameraBindGroupLayout!: GPUBindGroupLayout;

	public modelBindGroups: GPUBindGroup[] = [];
	public computeBindGroups: GPUBindGroup[] = [];
	public computeBindGroupLayout!: GPUBindGroupLayout;
	public backgroundBindGroupLayout!: GPUBindGroupLayout;
	public backgroundBindGroup!: GPUBindGroup;
	public mapEditorBindGroupLayout!: GPUBindGroupLayout;
	public mapEditorBindGroup!: GPUBindGroup;
	public patchBindGroupLayout!: GPUBindGroupLayout;
	public patchBindGroup!: GPUBindGroup;

	public modelBindGroupLayout!: GPUBindGroupLayout;
	private readonly device: GPUDevice;

	constructor(props: BindGroupManagerProps) {
		this.device = props.device;
	}

	public createBindGroupLayouts(): void {
		this.createCameraBindGroupLayout();
		this.createModelBindGroupLayout();
		this.createComputeBindGroupLayout();
		this.createBackgroundBindGroupLayout();
		this.createPatchBindGroupLayout();
	}

	public createCameraBindGroup(cameraUBO: GPUBuffer): void {
		this.cameraBindGroup = this.device.createBindGroup({
			layout: this.cameraBindGroupLayout,
			entries: [
				{
					binding: 0,
					resource: {
						buffer: cameraUBO,
						size:
							2 *
							renderEngineConfig.cameraDataFloatSize *
							Float32Array.BYTES_PER_ELEMENT,
					},
				},
			],
		});
	}

	public createModelBindGroup(entity: Entity, modelSBO: GPUBuffer): void {
		this.modelBindGroups.push(
			this.device.createBindGroup({
				label: entity.entityType,
				layout: this.modelBindGroupLayout,
				entries: [
					{
						binding: 0,
						resource: {
							buffer: modelSBO,
							size:
								renderEngineConfig.modelDataFloatSize *
								Float32Array.BYTES_PER_ELEMENT *
								renderEngineConfig.chunkSize,
						},
					},
				],
			}),
		);
	}

	public createPatchBindGroup(patchBuffer: GPUBuffer): void {
		this.patchBindGroup = this.device.createBindGroup({
			label: "patchBindGroup",
			layout: this.patchBindGroupLayout,
			entries: [
				{
					binding: 0,
					resource: {
						buffer: patchBuffer,
						size:
							renderEngineConfig.patchChunkSize *
							Float32Array.BYTES_PER_ELEMENT *
							renderEngineConfig.patchDataFloatSize,
					},
				},
			],
		});
	}

	public createComputeBindGroup(
		computeBuffers: GPUBuffer[],
		uniformBuffer: GPUBuffer,
		size: number,
	): void {
		this.computeBindGroups.push(
			this.device.createBindGroup({
				label: "compute BindGroup1",
				layout: this.computeBindGroupLayout,
				entries: [
					{
						binding: 0,
						resource: {
							buffer: computeBuffers[0],
							size: size * size * Uint32Array.BYTES_PER_ELEMENT,
						},
					},
					{
						binding: 1,
						resource: {
							buffer: computeBuffers[1],
							size: size * size * Uint32Array.BYTES_PER_ELEMENT,
						},
					},
					{
						binding: 2,
						resource: {
							buffer: uniformBuffer,
							size: 3 * Uint32Array.BYTES_PER_ELEMENT,
						},
					},
				],
			}),
		);
		this.computeBindGroups.push(
			this.device.createBindGroup({
				label: "compute BindGroup2",
				layout: this.computeBindGroupLayout,
				entries: [
					{
						binding: 0,
						resource: {
							buffer: computeBuffers[1],
							size: size * size * Uint32Array.BYTES_PER_ELEMENT,
						},
					},
					{
						binding: 1,
						resource: {
							buffer: computeBuffers[0],
							size: size * size * Uint32Array.BYTES_PER_ELEMENT,
						},
					},
					{
						binding: 2,
						resource: {
							buffer: uniformBuffer,
							size: 3 * Uint32Array.BYTES_PER_ELEMENT,
						},
					},
				],
			}),
		);
	}

	public createBackgroundBindGroup(backgroundBuffer: GPUBuffer): void {
		this.backgroundBindGroup = this.device.createBindGroup({
			label: "background bindgroup",
			layout: this.backgroundBindGroupLayout,
			entries: [
				{
					binding: 0,
					resource: {
						buffer: backgroundBuffer,
						size:
							renderEngineConfig.mapSize *
							renderEngineConfig.mapSize *
							Uint32Array.BYTES_PER_ELEMENT,
					},
				},
			],
		});
	}

	private createBackgroundBindGroupLayout(): void {
		this.backgroundBindGroupLayout = this.device.createBindGroupLayout({
			entries: [
				{
					binding: 0,
					visibility: GPUShaderStage.FRAGMENT,
					buffer: {
						type: "uniform",
					},
				},
			],
			label: "backgroundLayout",
		});
	}

	private createCameraBindGroupLayout(): void {
		this.cameraBindGroupLayout = this.device.createBindGroupLayout({
			entries: [
				{
					binding: 0,
					visibility: GPUShaderStage.VERTEX | GPUShaderStage.FRAGMENT,
					buffer: {
						type: "uniform",
					},
				},
			],
			label: "cameraLayout",
		});
	}

	private createPatchBindGroupLayout(): void {
		this.patchBindGroupLayout = this.device.createBindGroupLayout({
			entries: [
				{
					binding: 0,
					visibility: GPUShaderStage.VERTEX,
					buffer: {
						type: "uniform",
					},
				},
			],
			label: "patchLayout",
		});
	}

	private createModelBindGroupLayout(): void {
		this.modelBindGroupLayout = this.device.createBindGroupLayout({
			entries: [
				{
					binding: 0,
					visibility: GPUShaderStage.VERTEX,
					buffer: {
						type: "read-only-storage",
					},
				},
			],
			label: "modelLayout",
		});
	}

	private createComputeBindGroupLayout(): void {
		this.computeBindGroupLayout = this.device.createBindGroupLayout({
			entries: [
				{
					binding: 0,
					visibility: GPUShaderStage.COMPUTE,
					buffer: {
						type: "read-only-storage",
					},
				},
				{
					binding: 1,
					visibility: GPUShaderStage.COMPUTE,
					buffer: {
						type: "storage",
					},
				},
				{
					binding: 2,
					visibility: GPUShaderStage.COMPUTE,
					buffer: {
						type: "storage",
					},
				},
			],
			label: "compute layout",
		});
	}
}
