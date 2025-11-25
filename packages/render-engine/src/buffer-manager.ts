type BufferManagerProps = { device: GPUDevice };

export class BufferManager {
	public cameraUBO!: GPUBuffer;

	public vertexBuffers: GPUBuffer[];
	public modelSBOs: GPUBuffer[];
	public computeBuffers: GPUBuffer[];

	public computeUBO!: GPUBuffer;
	public readBuffer!: GPUBuffer;

	public backgroundBuffer!: GPUBuffer;
	public patchBuffer!: GPUBuffer;

	private readonly device: GPUDevice;

	constructor(props: BufferManagerProps) {
		this.device = props.device;

		this.vertexBuffers = [];
		this.modelSBOs = [];
		this.computeBuffers = [];
	}

	public createCameraBuffer(): GPUBuffer {
		this.cameraUBO = this.device.createBuffer({
			size:
				2 *
				renderEngineConfig.cameraDataFloatSize *
				Float32Array.BYTES_PER_ELEMENT,
			usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
		});

		return this.cameraUBO;
	}

	public createPatchBuffer(): GPUBuffer {
		this.patchBuffer = this.device.createBuffer({
			label: "Patch Buffer",
			size:
				Float32Array.BYTES_PER_ELEMENT *
				renderEngineConfig.patchChunkSize *
				renderEngineConfig.patchDataFloatSize,
			usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
		});
		return this.patchBuffer;
	}

	public createVertexBuffer(entity: Entity): void {
		const vertexBuffer = this.device.createBuffer({
			label: entity.entityType,
			size: entity.vertexData.length * Float32Array.BYTES_PER_ELEMENT,
			usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
		});

		this.vertexBuffers.push(vertexBuffer);

		this.device.queue.writeBuffer(vertexBuffer, 0, entity.vertexData);
	}

	public createModelSBO(entity: Entity): GPUBuffer {
		const modelSBO = this.device.createBuffer({
			label: entity.entityType,
			size:
				renderEngineConfig.modelDataFloatSize *
				Float32Array.BYTES_PER_ELEMENT *
				renderEngineConfig.chunkSize,
			usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST,
			mappedAtCreation: false,
		});

		this.modelSBOs.push(modelSBO);

		return modelSBO;
	}

	public createBackgroundBuffer(size: number): GPUBuffer {
		this.backgroundBuffer = this.device.createBuffer({
			label: "background buffer",
			size: size * size * Uint32Array.BYTES_PER_ELEMENT,
			usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
		});
		return this.backgroundBuffer;
	}

	public createComputeBuffers(size: number): void {
		this.computeBuffers.push(
			this.device.createBuffer({
				label: "ping-pong buffer1",
				size: size * size * Uint32Array.BYTES_PER_ELEMENT,
				usage: GPUBufferUsage.COPY_DST | GPUBufferUsage.STORAGE,
			}),
		);

		this.computeBuffers.push(
			this.device.createBuffer({
				label: "ping-pong buffer2",
				size: size * size * Uint32Array.BYTES_PER_ELEMENT,
				usage: GPUBufferUsage.COPY_SRC | GPUBufferUsage.STORAGE,
			}),
		);
		this.computeUBO = this.device.createBuffer({
			label: "compute UBO",
			size: 3 * Uint32Array.BYTES_PER_ELEMENT,
			usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST,
		});
		this.readBuffer = this.device.createBuffer({
			label: "compute read",
			size: size * size * Uint32Array.BYTES_PER_ELEMENT,
			usage: GPUBufferUsage.COPY_DST | GPUBufferUsage.MAP_READ,
		});
	}

	public deleteComputeBuffers(): void {
		this.computeBuffers.pop()?.destroy();

		this.computeBuffers.pop()?.destroy();
		this.backgroundBuffer;
	}

	public isEntityTypeInModelSBOs(entityType: EntityType): boolean {
		return this.modelSBOs.some((modelSBO) => modelSBO.label === entityType);
	}
}
