import type p5 from "p5";
import { BaseMixin } from "ts-mxn";
import type { Entity } from "../../entity.interface";
import { circuitBoardConfig } from "../circuit-board.config";
import {
	type CircuitBoardEntities,
	type ICircuitBoard,
	MouseInput,
	State,
} from "../circuit-board.interface";

export type IMouseInputService = {
	mouseClicked: () => void;
	mouseMoved: () => void;
	mouseDragged: () => void;
	mouseDoubleClicked: () => void;
	mouseReleased: () => void;
	isMouseOver: () => boolean;
	isMouseOverIOChipPanel(kind: "input" | "output"): boolean;
	getMouseOverEntity(entities: CircuitBoardEntities): Entity | undefined;
};

type IMouseInputServiceArgs = {
	p: p5;
	circuitBoard: ICircuitBoard;
};

class MouseInputService implements IMouseInputService {
	private p: p5;
	private circuitBoard: ICircuitBoard;

	private mouseReleaseAfterDrag: boolean;

	constructor(args: IMouseInputServiceArgs) {
		this.circuitBoard = args.circuitBoard;
		this.p = args.p;

		this.mouseReleaseAfterDrag = false;
	}

	public mouseClicked(): void {
		if (this.mouseReleaseAfterDrag) {
			this.mouseReleaseAfterDrag = false;
			return;
		}
		this.circuitBoard.getState().interact(MouseInput.Click);
	}

	public mouseMoved(): void {
		this.circuitBoard.getState().interact(MouseInput.Move);
	}

	public mouseDragged(): void {
		this.circuitBoard.getState().interact(MouseInput.Drag);
	}

	public mouseDoubleClicked(): void {
		this.circuitBoard.getState().interact(MouseInput.DoubleClick);
	}

	public mouseReleased(): void {
		if (this.circuitBoard.currentState === State.Reposition) {
			this.mouseReleaseAfterDrag = true;
			this.circuitBoard.setState({ state: State.Idle });
		}
	}

	public isMouseOver(): boolean {
		return (
			this.p.mouseX >= this.circuitBoard.position.x &&
			this.p.mouseX <=
				this.circuitBoard.position.x + this.circuitBoard.size.w &&
			this.p.mouseY >= this.circuitBoard.position.y &&
			this.p.mouseY <= this.circuitBoard.position.y + this.circuitBoard.size.h
		);
	}

	public isMouseOverIOChipPanel(kind: "input" | "output"): boolean {
		return kind === "input"
			? this.p.mouseX >= 0 &&
					this.p.mouseX <= circuitBoardConfig.widthScale / 2 &&
					this.p.mouseY >= this.circuitBoard.position.y &&
					this.p.mouseY <=
						this.circuitBoard.position.y + this.circuitBoard.size.h
			: // !this.isMouseOverlapping(this.inputs)
				this.p.mouseX >=
					this.circuitBoard.position.x +
						this.circuitBoard.size.w +
						circuitBoardConfig.widthScale / 2 &&
					this.p.mouseY >= this.circuitBoard.position.y &&
					this.p.mouseY <=
						this.circuitBoard.position.y + this.circuitBoard.size.h;
		// !this.isMouseOverlapping(this.inputs)
	}

	public getMouseOverEntity(
		entities: CircuitBoardEntities,
	): Entity | undefined {
		for (const entity of [
			...entities.inputs,
			...entities.outputs,
			...entities.chips,
			...entities.wires,
		]) {
			const mouseOverEntity = entity.isMouseOverGetEntity();
			if (mouseOverEntity) {
				return mouseOverEntity;
			}
		}
	}
}
type IMouseInputMixinArgs = Omit<IMouseInputServiceArgs, "circuitBoard">;

export class MouseInputMixin extends BaseMixin<
	ICircuitBoard,
	IMouseInputService
> {
	constructor(args: IMouseInputMixinArgs) {
		super({
			methods: [
				"getMouseOverEntity",
				"isMouseOver",
				"isMouseOverIOChipPanel",
				"mouseClicked",
				"mouseMoved",
				"mouseDragged",
				"mouseDoubleClicked",
				"mouseReleased",
			],
			props: [],
			initMixin: (circuitBoard) =>
				new MouseInputService({ circuitBoard, ...args }),
		});
	}
}
