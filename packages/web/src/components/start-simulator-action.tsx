import type * as React from "react";
import { SimulatorApp } from "@digital-logic-sim/simulator";
import { useEffectOnce } from "../utils";

type StartSimulatorActionProps = {
	canvas: HTMLCanvasElement;
	onSimulatorAppStart: (simulatorApp: SimulatorApp) => void;
};

export const StartSimulatorAction: React.FC<StartSimulatorActionProps> = ({
	canvas,
	onSimulatorAppStart,
}) => {
	useEffectOnce(() => {
		const simulatorApp = new SimulatorApp({ canvas });
		simulatorApp.start();

		onSimulatorAppStart(simulatorApp);

		// TODO: cleanup and stop simulator
		return () => {};
		// return () => simulatorApp.stop();
	});

	return null;
};
