import { ToolController } from "./tool-controller";
import { BaseLayer } from "../base-layer";
import type { Simulator } from "../../simulator";
import type { Renderable } from "@digital-logic-sim/render-engine";

export class InteractionLayer extends BaseLayer {
	private readonly toolController: ToolController;

	constructor(args: {
		sim: Simulator;
	}) {
		super(args);

		this.toolController = new ToolController();
	}

	public render(): void {
		const tool = this.toolController.getActiveTool();
		tool?.render();
	}

	public getRenderables(): Renderable[] {
		return [];
	}

	public onPointerMove(): void {}
}
