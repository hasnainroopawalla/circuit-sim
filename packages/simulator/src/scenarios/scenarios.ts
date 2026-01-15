/** biome-ignore-all lint/style/noNonNullAssertion: <explicit chip spawns> */
import type { Simulator } from "../simulator";

export const SCENARIOS = {
	NAND: (sim: Simulator) => {
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

		// INPUT 0
		const inputChip0 = sim.chipManager.spawnChip(inputChipFactory, {
			color: { r: 0, g: 0, b: 0, a: 1 },
			position: { x: 1.6, y: 1.3 },
		});
		// INPUT 1
		const inputChip1 = sim.chipManager.spawnChip(inputChipFactory, {
			color: { r: 0, g: 0, b: 0, a: 1 },
			position: { x: 1.6, y: 0.8 },
		});
		// AND
		const andChip = sim.chipManager.spawnChip(andChipFactory, {
			color: { r: 0, g: 0, b: 0, a: 1 },
			position: { x: 0.3, y: 1 },
		});
		// NOT
		const notChip = sim.chipManager.spawnChip(notChipFactory, {
			color: { r: 0, g: 0, b: 0, a: 1 },
			position: { x: -1, y: 1 },
		});
		// OUTPUT 0
		const outputChip = sim.chipManager.spawnChip(outputChipFactory, {
			color: { r: 0, g: 0, b: 0, a: 1 },
			position: { x: -2, y: 1 },
		});
		// input 0 to AND in 0
		sim.wireManager.spawnWire(
			{
				startPin: inputChip0.getPin(),
				endPin: andChip.getPin("in0")!,
			},
			{
				controlPoints: [],
			},
		);
		// input 1 to AND in 1
		sim.wireManager.spawnWire(
			{
				startPin: inputChip1.getPin(),
				endPin: andChip.getPin("in1")!,
			},
			{
				controlPoints: [],
			},
		);
		// AND out to NOT in
		sim.wireManager.spawnWire(
			{
				startPin: andChip.getPin("out0")!,
				endPin: notChip.getPin("in0")!,
			},
			{
				controlPoints: [],
			},
		);
		// NOT out to output
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
};
