type EntityType = "chip" | "pin" | "wire";

export abstract class Entity {
	public readonly id: string;

	constructor(args: { id: string; type: EntityType }) {
		this.id = args.id;
	}
}
