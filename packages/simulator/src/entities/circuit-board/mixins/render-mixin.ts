import type p5 from "p5";
import { BaseMixin } from "ts-mxn";
import { AbstractRenderer } from "../../abstract-renderer";
import type { Position, Size } from "../../types";
import { circuitBoardConfig } from "../circuit-board.config";
import type { ICircuitBoard } from "../circuit-board.interface";

export type IRenderService = {
	render: () => void;
	position: Position;
	size: Size<"rect">;
};

type IRenderServiceArgs = {
	circuitBoard: ICircuitBoard;
	p: p5;
	options: {
		position: Position;
		size: Size<"rect">;
	};
};

class RenderService
	extends AbstractRenderer<Size<"rect">>
	implements IRenderService
{
	private circuitBoard: ICircuitBoard;

	constructor(args: IRenderServiceArgs) {
		super({
			p: args.p,
			position: args.options.position,
			size: args.options.size,
		});
		this.circuitBoard = args.circuitBoard;
	}

	public render(): void {
		this.p.push();
		this.p.stroke(circuitBoardConfig.strokeColor);
		this.p.strokeWeight(2);
		this.p.fill(circuitBoardConfig.background);
		this.p.rect(this.position.x, this.position.y, this.size.w, this.size.h);
		// slider section
		this.p.strokeWeight(0);
		this.p.fill(circuitBoardConfig.sliderSectionColor);
		this.p.rect(
			0,
			this.position.y,
			circuitBoardConfig.widthScale / 2,
			this.size.h,
		);
		this.p.rect(
			this.position.x + this.size.w + circuitBoardConfig.widthScale / 2,
			this.position.y,
			circuitBoardConfig.widthScale / 2,
			this.size.h,
		);
		this.p.pop();

		this.circuitBoard.getState().render();

		this.renderWires();
		this.renderChips();
		this.renderIOChips();
	}

	private renderIOChips(): void {
		this.circuitBoard.entities.inputs.forEach((inputIOChip) => {
			inputIOChip.render();
		});
		this.circuitBoard.entities.outputs.forEach((outputIOChip) => {
			outputIOChip.render();
		});
	}

	private renderChips(): void {
		this.circuitBoard.entities.chips.forEach((chip) => {
			chip.render();
		});
	}

	private renderWires(): void {
		this.circuitBoard.entities.wires.forEach((wire) => {
			wire.render();
		});
	}
}

type IRenderMixinArgs = Omit<IRenderServiceArgs, "circuitBoard">;

export class RenderMixin extends BaseMixin<ICircuitBoard, IRenderService> {
	constructor(args: IRenderMixinArgs) {
		super({
			methods: ["render"],
			props: ["position", "size"],
			initMixin: (circuitBoard) => new RenderService({ circuitBoard, ...args }),
		});
	}
}
