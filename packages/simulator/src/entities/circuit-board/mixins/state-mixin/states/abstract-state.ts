import type p5 from "p5";
import type {
	ICircuitBoard,
	MouseInput,
} from "../../../circuit-board.interface";

export abstract class AbstractState {
	protected p: p5;
	protected circuitBoard: ICircuitBoard;

	constructor(p: p5, circuitBoard: ICircuitBoard) {
		this.p = p;
		this.circuitBoard = circuitBoard;
	}

	public render(): void {}

	public abstract interact(mouseInput: MouseInput): void;

	public abstract dispose(): void;
}
