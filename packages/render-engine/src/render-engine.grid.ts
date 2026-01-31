import { PipelineType } from "./pipeline-manager";
import type { RenderEngine } from "./render-engine";

export class GridRenderer {
	private renderEngine: RenderEngine;

	constructor(renderEngine: RenderEngine) {
		this.renderEngine = renderEngine;
	}

	public render(commandEncoder: GPUCommandEncoder): void {
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
			PipelineType.GridShader,
		);

		if (!pipeline) {
			throw new Error("GenericShader pipeline not initialized.");
		}

		passEncoder.setPipeline(pipeline);

		passEncoder.setBindGroup(
			0,
			this.renderEngine.view.bindGroupManager.cameraBindGroup,
		);
		passEncoder.draw(6, 1, 0, 0);
		passEncoder.end();
	}
}
