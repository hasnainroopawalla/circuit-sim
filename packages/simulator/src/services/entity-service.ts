import type { Entity } from "../entities/entity";
import type { Simulator } from "../simulator";
import { BaseService } from "./base-service";

export class EntityService extends BaseService {
	public readonly entities: Entity[];

	constructor(sim: Simulator) {
		super(sim);
		this.entities = [];
	}

	public add(entity: Entity): void {
		this.entities.push(entity);
	}

	public getEntityById(id: string): Entity | undefined {
		return this.entities.find((entity) => entity.id === id);
	}
}
