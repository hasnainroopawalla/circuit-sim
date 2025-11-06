import type { ToolController } from "../../tool-controller";
import { BaseLayer } from "../base-layer";

export class InteractionLayer extends BaseLayer {
	private readonly toolController: ToolController;

	constructor(args: {
		toolController: ToolController;
	}) {
		super();

		this.toolController = args.toolController;
	}

	public render(): void {
		const tool = this.toolController.getActiveTool();
		tool?.render();
	}
}
