import * as React from "react";
import { useSimulatorApp } from "../../contexts/simulator-app-context";
import type { OverlayElementKind } from "@digital-logic-sim/simulator";

export type Label = {
	id: string;
	text: string;
	kind: OverlayElementKind;
};

export const useOverlayLabels = () => {
	const simulatorApp = useSimulatorApp();

	const [labels, setLabels] = React.useState<Label[]>([]);

	React.useEffect(() => {
		const disposeSpawnChipSubscription = simulatorApp.sim.on(
			"chip.spawn.finish",
			({ chipId, chipName, pins }) => {
				setLabels((prev) => {
					const newLabels = [
						{
							id: chipId,
							text: chipName,
							kind: "static" as const,
						},
						...pins.map((pin) => ({
							id: pin.id,
							text: pin.name,
							kind: "hover" as const,
						})),
					];

					return [...prev, ...newLabels];
				});
			},
		);

		const disposeSimResetSubscription = simulatorApp.sim.on("sim.reset", () => {
			setLabels([]);
		});

		return () => {
			disposeSpawnChipSubscription();
			disposeSimResetSubscription();
		};
	}, [simulatorApp]);

	return labels;
};
