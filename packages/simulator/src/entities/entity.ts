type EntityType = "chip" | "pin" | "wire";

export abstract class Entity {
	public readonly id: string;
	public readonly type: EntityType;

	constructor(args: { id: string; type: EntityType }) {
		this.id = args.id;
		this.type = args.type;
	}
}
