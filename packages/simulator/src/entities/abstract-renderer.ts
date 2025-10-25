import type p5 from "p5";
import type { CircleSize, Position, RectSize } from "./types";

type IAbstractRendererArgs<T> = {
	p: p5;
	position: Position;
	size: T;
};

export abstract class AbstractRenderer<T extends RectSize | CircleSize> {
	p: p5;
	position: Position;
	size: T;

	constructor(args: IAbstractRendererArgs<T>) {
		this.p = args.p;
		this.position = args.position;
		this.size = args.size;
	}

	public abstract render(): void;
}
