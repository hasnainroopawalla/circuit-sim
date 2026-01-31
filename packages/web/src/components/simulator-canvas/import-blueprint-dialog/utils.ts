import type { Blueprint } from "@digital-logic-sim/simulator";
import type { PreviewNode } from "./blueprint-preview-tree";

export function blueprintStringToPreviewNode(
	blueprintString: string,
): PreviewNode {
	const blueprint = JSON.parse(blueprintString) as Blueprint;

	if (!blueprint.root || !blueprint.definitions) {
		throw new Error("Invalid blueprint");
	}

	const visited = new Set<string>();

	const buildNode = (name: string): PreviewNode => {
		if (visited.has(name)) {
			return { name, children: [] }; // prevent infinite loops
		}

		visited.add(name);

		const def = blueprint.definitions[name];
		if (!def) {
			throw new Error(`Missing definition for "${name}"`);
		}

		const children: PreviewNode[] = [];

		for (const chip of def.chips) {
			if (chip.spec.chipType === "composite") {
				children.push(buildNode(chip.spec.name));
			}
		}

		return { name, children };
	};

	const rootNode = buildNode(blueprint.root);
	rootNode.isRoot = true;

	return rootNode;
}
