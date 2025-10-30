import { Chip, type AtomicChipSpec } from "./chip";

export abstract class AtomicChip extends Chip {
	constructor(chipSpec: AtomicChipSpec) {
		super(chipSpec);
	}
}

export class AndChip extends AtomicChip {
	constructor(chipSpec: AtomicChipSpec) {
		super(chipSpec);
	}

	public execute(inputs: boolean[]): boolean[] {
		return [inputs[0] && inputs[1]];
	}
}

export class NotChip extends AtomicChip {
	constructor(chipSpec: AtomicChipSpec) {
		super(chipSpec);
	}

	public execute(inputs: boolean[]): boolean[] {
		return [!inputs[0]];
	}
}

export class InputChip extends AtomicChip {
	private state: boolean = false;

	constructor(chipSpec: AtomicChipSpec) {
		super(chipSpec);
	}

	public execute(): boolean[] {
		return [this.state]; // TODO, it should set the output pin state
	}
}

export class OutputChip extends AtomicChip {
	constructor(chipSpec: AtomicChipSpec) {
		super(chipSpec);
	}

	public execute(inputs: boolean[]): boolean[] {
		return [inputs[0]];
	}
}
