import type * as React from "react";
import { SimulatorApp } from "@digital-logic-sim/simulator";
import { useEffectOnce } from "../utils";

type StartSimulatorActionProps = {
	canvas: HTMLCanvasElement;
	onSimulatorAppStartSuccess: (simulatorApp: SimulatorApp) => void;
	onSimulatorAppStartFailure: (simulatorApp: SimulatorApp) => void;
};

export const StartSimulatorAction: React.FC<StartSimulatorActionProps> = ({
	canvas,
	onSimulatorAppStartSuccess,
	onSimulatorAppStartFailure,
}) => {
	useEffectOnce(() => {
		const simulatorApp = new SimulatorApp({ canvas });
		simulatorApp
			.start()
			.then(() => {
				onSimulatorAppStartSuccess(simulatorApp);
			})
			.catch(() => {
				onSimulatorAppStartFailure(simulatorApp);
			});

		// TODO: cleanup and stop simulator
		return () => {
			//simulatorApp.stop()
		};
	});

	return null;
};
