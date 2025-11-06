export abstract class Tool {
	constructor() {}

	public abstract render(): void;

	public abstract onPointerMove(event: Event): void;
}
