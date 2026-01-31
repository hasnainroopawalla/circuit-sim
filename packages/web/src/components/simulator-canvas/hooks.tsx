import * as React from "react";
import { useSimulatorApp } from "../../contexts/simulator-app-context";
import type {
	ChipType,
	OverlayElementKind,
} from "@digital-logic-sim/simulator";
import type { PinType } from "@digital-logic-sim/simulator";

export type Label = {
	id: string;
	text: string;
	kind: OverlayElementKind;
} & (
	| {
			entityType: "chip";
	  }
	| { entityType: "pin"; pinType: PinType }
);

export const useOverlayLabels = () => {
	const simulatorApp = useSimulatorApp();

	const [labels, setLabels] = React.useState<Label[]>([]);

	React.useEffect(() => {
		const disposeSpawnChipSubscription = simulatorApp.sim.on(
			"chip.spawn.finish",
			({ chipId, chipName, chipType, pins }) => {
				setLabels((prev) => {
					const newLabels: Label[] = [];

					if (shouldRegisterChipLabel(chipType)) {
						newLabels.push({
							id: chipId,
							entityType: "chip" as const,
							kind: "static" as const,
							text: chipName,
						});
					}

					newLabels.push(
						...pins.map((pin) => ({
							id: pin.id,
							entityType: "pin" as const,
							kind: "hover" as const,
							text: pin.name,
							pinType: pin.pinType,
						})),
					);

					return [...prev, ...newLabels];
				});
			},
		);

		const disposeOverlayResetSubscription = simulatorApp.sim.on(
			"overlay.reset",
			() => {
				setLabels([]);
			},
		);

		return () => {
			disposeSpawnChipSubscription();
			disposeOverlayResetSubscription();
		};
	}, [simulatorApp]);

	return labels;
};

function shouldRegisterChipLabel(chipType: ChipType): boolean {
	return chipType !== "io";
}
