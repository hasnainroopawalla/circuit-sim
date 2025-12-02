import type { Chip } from "./chips";
import type { Pin } from "./pin";
import type { Wire } from "./wire";

type EntityType = "chip" | "pin" | "wire";

export type Entity = Chip | Wire | Pin;

export abstract class BaseEntity<T extends EntityType> {
	public readonly id: string;
	public readonly entityType: T;

	protected constructor(args: { id: string; entityType: T }) {
		this.id = args.id;
		this.entityType = args.entityType;
	}
}
