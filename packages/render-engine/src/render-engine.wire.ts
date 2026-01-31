import { PipelineType } from "./pipeline-manager";
import type { WireRenderable } from "./render-engine.interface";
import { renderEngineConfig } from "./render-engine.config";
import type { RenderEngine } from "./render-engine";
import { mat4, vec3, vec4 } from "wgpu-matrix";

export class WireRenderer {
	private renderEngine: RenderEngine;

	constructor(renderEngine: RenderEngine) {
		this.renderEngine = renderEngine;
	}

	public render(
		commandEncoder: GPUCommandEncoder,
		wires: WireRenderable[],
	): void {
		const wireCount = this.uploadWireRenderData(wires);
		this.wireRenderPass(commandEncoder, wireCount);
	}

	//private uploadWireRenderData(wireData: WireRenderable[]): number {
	//	const lineVertexData = new Float32Array(
	//		renderEngineConfig.chunkSize * renderEngineConfig.lineDataFloatSize,
	//	);

	//	const offset = this.getWireControlPoints(wireData).reduce(
	//		(offset, path) => {
	//			for (let i = 1; i < path.length / 2; ++i) {
	//				const start = path.subarray(2 * (i - 1), 2 * i);
	//				lineVertexData.set(start, offset + 4 * (i - 1));

	//				const end = path.subarray(2 * i, 2 * (i + 1));
	//				lineVertexData.set(end, offset + 4 * i - 2);
	//			}

	//			return offset + 2 * path.length - 4;
	//		},
	//		0 /* initial value */,
	//	);

	//	this.renderEngine.view.device.queue.writeBuffer(
	//		this.renderEngine.view.bufferManager.vertexBuffers[0],
	//		0 /* bufferOffset */,
	//		lineVertexData,
	//		0 /* dataOffset */,
	//		offset * Float32Array.BYTES_PER_ELEMENT,
	//	);
	//	return offset / 2; /* number of wire vertrices */
	//}

	private uploadWireRenderData(wireData: WireRenderable[]): number {
		const lineModelData = new Float32Array(
			renderEngineConfig.chunkSize * (renderEngineConfig.modelFloatSize - 4),
		);

		const offset = wireData.reduce((offset, wire) => {
			for (let i = 1; i < wire.path.length; ++i) {
				const start = wire.path[i - 1];
				const end = wire.path[i];
				const wireLength = Math.sqrt(
					(end.x - start.x) ** 2 + (end.y - start.y) ** 2,
				);
				const scaleMat = mat4.scaling(
					vec3.create(wireLength / 2, renderEngineConfig.lineThickness, 1),
				);
				const lineCenter = vec3.create(
					(end.x + start.x) / 2,
					(end.y + start.y) / 2,
					0,
				);
				const translateMat = mat4.translation(lineCenter);
				const colour = vec4.create(
					wire.color.r,
					wire.color.g,
					wire.color.b,
					1.0,
				);
				let lineAngle = Math.PI / 2;
				if (end.x !== start.x) {
					lineAngle = Math.atan((end.y - start.y) / (end.x - start.x));
				}
				const rotationMat = mat4.rotationZ(lineAngle);
				let modelMatrix = mat4.multiply(rotationMat, scaleMat);
				modelMatrix = mat4.multiply(translateMat, modelMatrix);
				const localOffset = (i - 1) * renderEngineConfig.modelFloatSize;
				lineModelData.set(modelMatrix, offset + localOffset);
				lineModelData.set(
					colour,
					offset + localOffset + renderEngineConfig.matrixFloatSize,
				);
			}

			return offset + renderEngineConfig.modelFloatSize * wire.path.length;
		}, 0 /* initial value */);

		this.renderEngine.view.device.queue.writeBuffer(
			this.renderEngine.view.bufferManager.modelSBOs[1],
			0 /* bufferOffset */,
			lineModelData,
			0 /* dataOffset */,
			offset * Float32Array.BYTES_PER_ELEMENT,
		);
		return (
			offset / renderEngineConfig.modelFloatSize
		); /* number of wire instances */
	}

	private wireRenderPass(
		commandEncoder: GPUCommandEncoder,
		wireCount: number,
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
		passEncoder.setBindGroup(
			1,
			this.renderEngine.view.bindGroupManager.modelBindGroups[1],
		);
		passEncoder.draw(6, wireCount, 0, 0);
		passEncoder.end();
	}

	private getWireControlPoints(wireData: WireRenderable[]): Float32Array[] {
		return wireData.map((wire) => {
			const path = new Float32Array(2 * wire.path.length);
			for (let i = 0; i < wire.path.length; ++i) {
				path.set([wire.path[i].x, wire.path[i].y], i * 2);
			}

			return path;
		});
	}
}
