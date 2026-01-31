import { renderEngineConfig } from "./render-engine.config";

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
							renderEngineConfig.matrixFloatSize *
							Float32Array.BYTES_PER_ELEMENT,
					},
				},
			],
		});
	}

	public createModelBindGroup(modelSBO: GPUBuffer, auxilaryDataFloatSize: number): void {
		this.modelBindGroups.push(
			this.device.createBindGroup({
				label: "chip", // TODO: dont hardcode chip
				layout: this.modelBindGroupLayout,
				entries: [
					{
						binding: 0,
						resource: {
							buffer: modelSBO,
							size:
								(renderEngineConfig.modelFloatSize + auxilaryDataFloatSize) *
								Float32Array.BYTES_PER_ELEMENT *
								renderEngineConfig.chunkSize,
						},
					},
				],
			}),
		);
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
}
