import * as React from "react";
import { useSimulatorApp } from "../../contexts/simulator-app-context";
import type { OverlayLabelData } from "@digital-logic-sim/simulator";

export const useOverlayLabels = () => {
	const simulatorApp = useSimulatorApp();

	const [labels, setLabels] = React.useState<OverlayLabelData[]>([]);

	React.useEffect(() => {
		const disposeOverlayChangedSubscription = simulatorApp.sim.on(
			"overlay.changed",
			() => {
				const labels = simulatorApp.overlayManager.getLabels();
				setLabels(labels);
			},
		);

		return () => {
			disposeOverlayChangedSubscription();
		};
	}, [simulatorApp]);

	return labels;
};
