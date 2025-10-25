import type p5 from "p5";
import { Chip, IOChip } from "../../../../chips";
import {
	type ICircuitBoard,
	MouseInput,
	State,
} from "../../../circuit-board.interface";
import { AbstractState } from "./abstract-state";

export class RepositionState extends AbstractState {
	private chip?: Chip | IOChip;

	constructor(p: p5, circuitBoard: ICircuitBoard) {
		super(p, circuitBoard);
	}

	public setup(chip: Chip | IOChip): void {
		this.chip = chip;
	}

	public interact(mouseInput: MouseInput) {
		switch (mouseInput) {
			case MouseInput.Drag:
				this.handleMouseDrag();
				break;
		}
	}

	public dispose(): void {
		this.chip = undefined;
	}

	private handleMouseDrag(): void {
		if (this.chip instanceof Chip && this.circuitBoard.isMouseOver()) {
			this.chip.mouseDragged();
		} else if (this.chip instanceof IOChip) {
			this.chip.mouseDragged();
		} else {
			this.circuitBoard.setState({ state: State.Idle });
		}
	}
}
