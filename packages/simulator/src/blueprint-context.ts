import type {
	CompositeDefinition,
	Blueprint,
} from "./services/blueprint-service";

export class BlueprintContext {
	private definitions: Map<string, CompositeDefinition>;

	constructor() {
		this.definitions = new Map<string, CompositeDefinition>();
	}

	public update(blueprint: Blueprint): void {
		this.definitions.clear();

		Object.entries(blueprint.definitions).forEach(([name, def]) => {
			this.definitions.set(name, def);
		});
	}

	public get(name: string): CompositeDefinition | undefined {
		return this.definitions.get(name);
	}
}
