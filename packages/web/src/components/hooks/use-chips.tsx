import * as React from "react";
import { colorGenerator } from "../color-generator";
import { useEventListener } from "./use-event-listener";
import { useSimulator } from "../simulator-context";

type IUseChipsState = { name: string; onClick: () => void };

export const useChips = () => {
	const simulator = useSimulator();

	const [chips, setChips] = React.useState<IUseChipsState[]>(() =>
		simulator.chipLibraryService.getAll().map((spec) => ({
			name: spec.label,
			onClick: () => simulator.emit("chip.spawn", spec),
		})),
	);

	const newChipData = useEventListener("AddCircuitChipToToolbar");

	React.useEffect(() => {
		if (!newChipData) {
			return;
		}
		const color = colorGenerator.generate();

		const chip = {
			name: newChipData.name,
			onClick: () => {},
			// pubsub.publish("SpawnChip", {
			// 	kind: "circuit",
			// 	name: newChipData.name,
			// 	blueprint: newChipData.blueprint,
			// 	color,
			// }),
		};
		setChips((prevCircuitChips) => [...prevCircuitChips, chip]);
	}, [newChipData]);

	return chips;
};
