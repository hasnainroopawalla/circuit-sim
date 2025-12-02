import { PipelineType } from "./pipeline-manager";
import type { WireRenderable } from "./render-engine.interface";
import { renderEngineConfig } from "./render-engine.config";
import type { RenderEngine } from "./render-engine";

export class WireRenderer {
	private renderEngine: RenderEngine;

	constructor(renderEngine: RenderEngine) {
		this.renderEngine = renderEngine;
	}

	public render(
		commandEncoder: GPUCommandEncoder,
		wires: WireRenderable[],
	): void {
		const wireVertexCount = this.uploadWireRenderData(wires);
		this.wireRenderPass(commandEncoder, wireVertexCount);
	}

	private uploadWireRenderData(wireData: WireRenderable[]): number {
		const lineVertexData = new Float32Array(
			renderEngineConfig.chunkSize * renderEngineConfig.lineDataFloatSize,
		);

		const offset = this.getWireControlPoints(wireData).reduce(
			(offset, controlPoints) => {
				for (let i = 1; i < controlPoints.length / 2; ++i) {
					const start = controlPoints.subarray(2 * (i - 1), 2 * i);
					lineVertexData.set(start, offset + 4 * (i - 1));

					const end = controlPoints.subarray(2 * i, 2 * (i + 1));
					lineVertexData.set(end, offset + 4 * i - 2);
				}

				return offset + 2 * controlPoints.length - 4;
			},
			0 /* initial value */,
		);

		this.renderEngine.view.device.queue.writeBuffer(
			this.renderEngine.view.bufferManager.vertexBuffers[0],
			0 /* bufferOffset */,
			lineVertexData,
			0 /* dataOffset */,
			offset * Float32Array.BYTES_PER_ELEMENT,
		);
		return offset / 2; /* number of wire vertrices */
	}

	private wireRenderPass(
		commandEncoder: GPUCommandEncoder,
		wireVertexCount: number,
	): void {
		const passEncoder = commandEncoder.beginRenderPass({
			colorAttachments: [
				{
					view: this.renderEngine.view.renderTargetView,
					clearValue: { r: 0, g: 0, b: 0, a: 1.0 },
					loadOp: "load",
					storeOp: "store",
				},
			],
			depthStencilAttachment: {
				view: this.renderEngine.view.depthView,
				depthClearValue: 1.0,
				depthLoadOp: "load",
				depthStoreOp: "store",
			},
		});

		const pipeline = this.renderEngine.view.pipelineManager.getPipeline(
			PipelineType.LineShader,
		);

		if (!pipeline) {
			throw new Error("LineShader pipeline not initialized.");
		}

		passEncoder.setPipeline(pipeline);

		passEncoder.setBindGroup(
			0,
			this.renderEngine.view.bindGroupManager.cameraBindGroup,
		);
		passEncoder.setVertexBuffer(
			0,
			this.renderEngine.view.bufferManager.vertexBuffers[0],
		);
		passEncoder.draw(wireVertexCount, 1, 0, 0);
		passEncoder.end();
	}

	private getWireControlPoints(wireData: WireRenderable[]): Float32Array[] {
		return wireData.map((wire) => {
			const controlPoints = new Float32Array(2 * wire.controlPoints.length);
			for (let i = 0; i < wire.controlPoints.length; ++i) {
				controlPoints.set(
					[wire.controlPoints[i].x, wire.controlPoints[i].y],
					i * 2,
				);
			}

			return controlPoints;
		});
	}
}
