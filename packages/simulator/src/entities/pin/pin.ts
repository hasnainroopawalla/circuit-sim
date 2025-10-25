import type p5 from "p5";
import { type Chip, IOChip } from "../chips";
import type { Position } from "../types";
import type { Wire } from "../wire";
import { PinRenderer } from "./pin.renderer";

export enum PinState {
	Low,
	High,
}

export class Pin {
	p: p5;
	name: string;
	id: number;
	state: PinState;
	isInput: boolean; // TODO: change to type: "input" | "output"
	isGhost: boolean;
	outgoingWires: Wire[];
	chip: Chip | IOChip;

	renderer: PinRenderer;

	constructor(
		p: p5,
		id: number,
		state: PinState,
		isInput: boolean,
		chip: Chip | IOChip,
		isGhost = false,
	) {
		this.p = p;
		this.id = id;
		this.state = state;
		this.isInput = isInput;
		this.outgoingWires = [];
		this.chip = chip;
		this.name =
			this.chip instanceof IOChip ? "pin" : `${isInput ? "In" : "Out"} ${id}`;
		this.isGhost = isGhost;

		this.renderer = new PinRenderer({ p, pin: this });
	}

	public isMouseOver(): boolean {
		return this.renderer.isMouseOver();
	}

	public propagate(): void {
		for (const outgoingWire of this.outgoingWires) {
			outgoingWire.propagate();
		}
	}

	public setPosition(position: Position): void {
		this.renderer.setPosition(position);
	}

	public mouseClicked(): boolean {
		return this.isMouseOver();
	}

	public render(): void {
		this.renderer.render();
	}
}
