import { PipelineType } from "./pipeline-manager";
import type { ChipRenderable } from "./render-engine.interface";
import { mat4, vec3, vec4 } from "wgpu-matrix";
import { renderEngineConfig } from "./render-engine.config";
import type { RenderEngine } from "./render-engine";

export class ChipRenderer {
	private renderEngine: RenderEngine;

	constructor(renderEngine: RenderEngine) {
		this.renderEngine = renderEngine;
	}

	public render(
		commandEncoder: GPUCommandEncoder,
		chips: ChipRenderable[],
	): void {
		const numChips = this.uploadChipRenderData(chips);
		this.chipRenderPass(commandEncoder, numChips /* chipCount */);
	}

	private uploadChipRenderData(chipData: ChipRenderable[]): number {
		const modelMatrixData = new Float32Array(
			renderEngineConfig.chunkSize *
				(renderEngineConfig.matrixFloatSize +
					renderEngineConfig.colorFloatSize),
		);

		const offset = chipData.reduce(
			(offset, chip) => this.generateChipMesh(chip, modelMatrixData, offset),
			0 /* initial value */,
		);

		this.renderEngine.view.bufferManager.modelSBOs.forEach((modelSBO) => {
			this.renderEngine.view.device.queue.writeBuffer(
				modelSBO,
				0 /* bufferOffset */,
				modelMatrixData,
				0 /*dataOffset */,
				offset * Float32Array.BYTES_PER_ELEMENT,
			);
		});
		return offset / renderEngineConfig.modelFloatSize;
	}

	private chipRenderPass(
		commandEncoder: GPUCommandEncoder,
		chipCount: number,
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
			PipelineType.GenericShader,
		);

		if (!pipeline) {
			throw new Error("GenericShader pipeline not initialized.");
		}

		passEncoder.setPipeline(pipeline);

		passEncoder.setBindGroup(
			0,
			this.renderEngine.view.bindGroupManager.cameraBindGroup,
		);
		passEncoder.setBindGroup(
			1,
			this.renderEngine.view.bindGroupManager.modelBindGroups[0],
		);
		passEncoder.draw(6, chipCount, 0, 0);
		passEncoder.end();
	}

	private generateChipMesh(
		chip: ChipRenderable,
		modelMatrixData: Float32Array,
		offset: number,
	): number {
		const maxPins = Math.max(chip.outputPins.length, chip.inputPins.length);

		const height = (maxPins * 1.5 + 0.5) * renderEngineConfig.pinSize;
		const width = renderEngineConfig.chipAspectRatio * height;

		const translate = mat4.translate(
			mat4.identity(),
			vec3.create(chip.position.x, chip.position.y),
		);
		const scale = mat4.scale(mat4.identity(), vec3.create(width, height, 1.0));
		modelMatrixData.set(mat4.multiply(translate, scale), offset);
		offset += renderEngineConfig.matrixFloatSize;
		modelMatrixData.set(
			vec4.create(chip.color.r, chip.color.g, chip.color.b, chip.color.a),
			offset,
		);
		offset += renderEngineConfig.colorFloatSize;

		const inputPinOffset = {
			x: chip.position.x + width,
			y:
				chip.position.y +
				height -
				renderEngineConfig.pinSize *
					(2 + (3 * (maxPins - chip.inputPins.length)) / 2),
		};
		const outputPinOffset = {
			x: chip.position.x - width,
			y:
				chip.position.y +
				height -
				renderEngineConfig.pinSize *
					(2 + (3 * (maxPins - chip.outputPins.length)) / 2),
		};

		for (let i = 0; i < chip.inputPins.length; ++i) {
			const pinPosition = {
				x: inputPinOffset.x,
				y: inputPinOffset.y - renderEngineConfig.pinSize * 3 * i,
			};
			const pinColour = {
				r: Number(!chip.inputPins[i].value),
				g: Number(chip.inputPins[i].value),
				b: 0.0,
				a: 1.0,
			};
			const translate = mat4.translate(
				mat4.identity(),
				vec3.create(pinPosition.x, pinPosition.y, -0.001),
			);
			const scale = mat4.scale(
				mat4.identity(),
				vec3.create(
					renderEngineConfig.pinSize,
					renderEngineConfig.pinSize,
					1.0,
				),
			);
			modelMatrixData.set(mat4.multiply(translate, scale), offset);
			offset += renderEngineConfig.matrixFloatSize;
			modelMatrixData.set(
				vec4.create(pinColour.r, pinColour.g, pinColour.b, pinColour.a),
				offset,
			);
			offset += renderEngineConfig.colorFloatSize;
		}
		for (let i = 0; i < chip.outputPins.length; ++i) {
			const pinPosition = {
				x: outputPinOffset.x,
				y: outputPinOffset.y - renderEngineConfig.pinSize * (3 * i),
			};
			const pinColour = {
				r: Number(!chip.outputPins[i].value),
				g: Number(chip.outputPins[i].value),
				b: 0.0,
				a: 1.0,
			};
			const translate = mat4.translate(
				mat4.identity(),
				vec3.create(pinPosition.x, pinPosition.y, -0.001),
			);
			const scale = mat4.scale(
				mat4.identity(),
				vec3.create(
					renderEngineConfig.pinSize,
					renderEngineConfig.pinSize,
					1.0,
				),
			);
			modelMatrixData.set(mat4.multiply(translate, scale), offset);
			offset += renderEngineConfig.matrixFloatSize;
			modelMatrixData.set(
				vec4.create(pinColour.r, pinColour.g, pinColour.b, pinColour.a),
				offset,
			);
			offset += renderEngineConfig.colorFloatSize;
		}
		return offset;
	}
}
