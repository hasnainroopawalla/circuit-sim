/** biome-ignore-all lint/style/noNonNullAssertion: <explicit chip spawns> */
import type { Simulator } from "../simulator";
import threeAndBp from "../blueprints/3-and.json";
import orUsingNandBp from "../blueprints/nand.json";
import type { Blueprint } from "../services/blueprint-service";
import type { Position } from "@digital-logic-sim/shared-types";

export const SCENARIOS = {
	Nand: (sim: Simulator) => {
		const andChipFactory = sim.chipLibraryService.getChipFactory({
			kind: "atomic",
			name: "AND",
		});

		const notChipFactory = sim.chipLibraryService.getChipFactory({
			kind: "atomic",
			name: "NOT",
		});

		const inputChipFactory = sim.chipLibraryService.getChipFactory({
			kind: "io",
			name: "input",
		});

		const outputChipFactory = sim.chipLibraryService.getChipFactory({
			kind: "io",
			name: "output",
		});

		const inputChip0 = sim.chipManager.spawnChip(inputChipFactory, {
			position: { x: 1.6, y: 1.3 },
		});

		const inputChip1 = sim.chipManager.spawnChip(inputChipFactory, {
			position: { x: 1.6, y: 0.8 },
		});

		const andChip = sim.chipManager.spawnChip(andChipFactory, {
			position: { x: 0.3, y: 1 },
		});

		const notChip = sim.chipManager.spawnChip(notChipFactory, {
			position: { x: -1, y: 1 },
		});

		const outputChip = sim.chipManager.spawnChip(outputChipFactory, {
			position: { x: -2, y: 1 },
		});

		sim.wireManager.spawnWire(
			{
				startPin: inputChip0.getPin(),
				endPin: andChip.getPin("in0")!,
			},
			{
				controlPoints: [],
			},
		);

		sim.wireManager.spawnWire(
			{
				startPin: inputChip1.getPin(),
				endPin: andChip.getPin("in1")!,
			},
			{
				controlPoints: [],
			},
		);

		sim.wireManager.spawnWire(
			{
				startPin: andChip.getPin("out0")!,
				endPin: notChip.getPin("in0")!,
			},
			{
				controlPoints: [],
			},
		);

		sim.wireManager.spawnWire(
			{
				startPin: notChip.getPin("out0")!,
				endPin: outputChip.getPin(),
			},
			{
				controlPoints: [],
			},
		);
	},

	ChipGallery: (sim: Simulator) => {
		sim.chipManager.spawnChip(
			sim.chipLibraryService.getChipFactory({
				kind: "atomic",
				name: "AND",
			}),
			{
				position: { x: 4, y: 2 },
			},
		);

		sim.chipManager.spawnChip(
			sim.chipLibraryService.getChipFactory({
				kind: "atomic",
				name: "OR",
			}),
			{
				position: { x: 3, y: 2 },
			},
		);

		sim.chipManager.spawnChip(
			sim.chipLibraryService.getChipFactory({
				kind: "atomic",
				name: "NOT",
			}),
			{
				position: { x: 2, y: 2 },
			},
		);

		loadAndSpawnCompositeChip(sim, threeAndBp as Blueprint, { x: 4, y: 1 });
		loadAndSpawnCompositeChip(
			sim,
			sim.blueprintService.renameBlueprint(
				threeAndBp as Blueprint,
				"this a very long name",
			),
			{ x: 2, y: 1 },
		);
		loadAndSpawnCompositeChip(
			sim,
			sim.blueprintService.renameBlueprint(threeAndBp as Blueprint, "hi"),
			{ x: 0, y: 1 },
		);
		loadAndSpawnCompositeChip(
			sim,
			sim.blueprintService.renameBlueprint(
				threeAndBp as Blueprint,
				"a very very very very very very very very long name",
			),
			{ x: 2, y: -0.5 },
		);
	},

	OrUsingNand: (sim: Simulator) => {
		const inputChipFactory = sim.chipLibraryService.getChipFactory({
			kind: "io",
			name: "input",
		});

		const outputChipFactory = sim.chipLibraryService.getChipFactory({
			kind: "io",
			name: "output",
		});

		const inputChip0 = sim.chipManager.spawnChip(inputChipFactory, {
			position: { x: 2.5, y: 2 },
		});

		const inputChip1 = sim.chipManager.spawnChip(inputChipFactory, {
			position: { x: 2.5, y: 1 },
		});

		const outputChip1 = sim.chipManager.spawnChip(outputChipFactory, {
			position: { x: -2, y: 1.5 },
		});

		sim.blueprintService.loadBlueprint(
			JSON.stringify(orUsingNandBp as Blueprint),
		);

		const nand1 = sim.chipManager.spawnChip(
			sim.chipLibraryService.getChipFactory({
				kind: "composite",
				name: "NAND",
			}),
			{
				position: { x: 1, y: 2 },
			},
		);

		const nand2 = sim.chipManager.spawnChip(
			sim.chipLibraryService.getChipFactory({
				kind: "composite",
				name: "NAND",
			}),
			{
				position: { x: 1, y: 1 },
			},
		);

		const nand3 = sim.chipManager.spawnChip(
			sim.chipLibraryService.getChipFactory({
				kind: "composite",
				name: "NAND",
			}),
			{
				position: { x: -0.5, y: 1.5 },
			},
		);

		sim.wireManager.spawnWire(
			{
				startPin: inputChip0.getPin(),
				endPin: nand1.getPin("IN 1")!,
			},
			{
				controlPoints: [],
			},
		);

		sim.wireManager.spawnWire(
			{
				startPin: inputChip0.getPin(),
				endPin: nand1.getPin("IN 2")!,
			},
			{
				controlPoints: [],
			},
		);

		sim.wireManager.spawnWire(
			{
				startPin: inputChip1.getPin(),
				endPin: nand2.getPin("IN 1")!,
			},
			{
				controlPoints: [],
			},
		);

		sim.wireManager.spawnWire(
			{
				startPin: inputChip1.getPin(),
				endPin: nand2.getPin("IN 2")!,
			},
			{
				controlPoints: [],
			},
		);

		sim.wireManager.spawnWire(
			{
				startPin: nand1.getPin("OUT 1")!,
				endPin: nand3.getPin("IN 1")!,
			},
			{
				controlPoints: [],
			},
		);

		sim.wireManager.spawnWire(
			{
				startPin: nand2.getPin("OUT 1")!,
				endPin: nand3.getPin("IN 2")!,
			},
			{
				controlPoints: [],
			},
		);

		sim.wireManager.spawnWire(
			{
				startPin: nand3.getPin("OUT 1")!,
				endPin: outputChip1.getPin(),
			},
			{
				controlPoints: [],
			},
		);
	},
};

function loadAndSpawnCompositeChip(
	sim: Simulator,
	blueprint: Blueprint,
	position: Position,
) {
	sim.blueprintService.loadBlueprint(JSON.stringify(blueprint));

	sim.chipManager.spawnChip(
		sim.chipLibraryService.getChipFactory({
			kind: "composite",
			name: blueprint.root,
		}),
		{
			position,
		},
	);
}
