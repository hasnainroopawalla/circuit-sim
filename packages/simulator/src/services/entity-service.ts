import type { Entity } from "../entities-new/entity";
import type { Simulator } from "../simulator";
import { BaseService } from "./base-service";

export class EntityService extends BaseService {
	protected readonly entities: Entity[];

	constructor(sim: Simulator) {
		super(sim);
		this.entities = [];
	}

	public add(entity: Entity): void {
		this.entities.push(entity);
	}
}
