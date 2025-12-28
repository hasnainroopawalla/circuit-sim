import * as React from "react";
import { useSimulatorApp } from "../../contexts/simulator-app-context";
import type { OverlayElementKind } from "@digital-logic-sim/simulator";
import type { PinType } from "@digital-logic-sim/simulator";

export type Label = {
	id: string;
	text: string;
	kind: OverlayElementKind;
} & ({
	entityType: "chip"
} | {entityType: "pin",  pinType: PinType});
 
export const useOverlayLabels = () => {
	const simulatorApp = useSimulatorApp();

	const [labels, setLabels] = React.useState<Label[]>([]);

	React.useEffect(() => {
		const disposeSpawnChipSubscription = simulatorApp.sim.on(
			"chip.spawn.finish",
			({ chipId, chipName, pins }) => {
				setLabels((prev) => {
					const newLabels:Label[] = [
						{
							id: chipId,
							entityType: "chip" as const,
							kind: "static" as const,
							text: chipName,
						},
						...pins.map((pin) => ({
							id: pin.id,
							entityType: "pin" as const,
							kind: "hover" as const,
							text: pin.name,
							pinType: pin.pinType
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
