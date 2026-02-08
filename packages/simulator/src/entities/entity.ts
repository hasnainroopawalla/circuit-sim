import type { Chip } from "./chips";
import type { Pin } from "./pin";
import type { Wire } from "./wire";

export enum EntityType {
	Chip = "Chip",
	Pin = "Pin",
	Wire = "Wire",
}

export type Entity = Chip | Wire | Pin;

export abstract class BaseEntity<T extends EntityType> {
	public id!: string;
	public entityType: T;

	protected constructor(args: { entityType: T }) {
		this.entityType = args.entityType;
	}

	public setId(id: string) {
		this.id = id;
	}
}
