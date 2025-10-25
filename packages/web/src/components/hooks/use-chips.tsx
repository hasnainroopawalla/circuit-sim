import { pubsub } from "@circuit-sim/pubsub";
import * as React from "react";
import { colorGenerator } from "../color-generator";
import { useEventListener } from "./use-event-listener";

type IUseChipsState = { name: string; onClick: () => void };

export const useChips = () => {
	const [chips, setChips] = React.useState<IUseChipsState[]>([
		{
			name: "AND",
			onClick: () =>
				pubsub.publish("SpawnChip", {
					kind: "core",
					name: "AND",
				}),
		},
		{
			name: "OR",
			onClick: () =>
				pubsub.publish("SpawnChip", {
					kind: "core",
					name: "OR",
				}),
		},
		{
			name: "NOT",
			onClick: () =>
				pubsub.publish("SpawnChip", {
					kind: "core",
					name: "NOT",
				}),
		},
	]);

	const newChipData = useEventListener("AddCircuitChipToToolbar");

	React.useEffect(() => {
		if (!newChipData) {
			return;
		}
		const color = colorGenerator.generate();

		const chip = {
			name: newChipData.name,
			onClick: () =>
				pubsub.publish("SpawnChip", {
					kind: "circuit",
					name: newChipData.name,
					blueprint: newChipData.blueprint,
					color,
				}),
		};
		setChips((prevCircuitChips) => [...prevCircuitChips, chip]);
	}, [newChipData]);

	return chips;
};
