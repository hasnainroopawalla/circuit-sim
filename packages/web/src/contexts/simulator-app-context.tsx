import * as React from "react";
import { SimulatorApp } from "@digital-logic-sim/simulator";

type SimulatorContextValue = {
	startSimulator: (canvas: HTMLCanvasElement) => void;
	stopSimulator: () => void;
	simulatorApp: SimulatorApp | null;
	isSimulatorRunning: boolean;
};

const SimulatorContext = React.createContext<SimulatorContextValue | null>(
	null,
);

export const SimulatorAppProvider = ({ children }: React.PropsWithChildren) => {
	const simulatorAppRef = React.useRef<SimulatorApp | null>(null);

	const [isSimulatorRunning, setIsSimulatorRunning] = React.useState(false);

	const startSimulator = React.useCallback((canvas: HTMLCanvasElement) => {
		if (simulatorAppRef.current) {
			return;
		}

		const app = new SimulatorApp({ canvas });
		app.start();

		simulatorAppRef.current = app;
		setIsSimulatorRunning(true);
	}, []);

	const stopSimulator = React.useCallback(() => {
		if (!simulatorAppRef.current) {
			return;
		}

		simulatorAppRef.current.stop();
		simulatorAppRef.current = null;
		setIsSimulatorRunning(false);
	}, []);

	return (
		<SimulatorContext.Provider
			value={{
				simulatorApp: simulatorAppRef.current,
				startSimulator,
				stopSimulator,
				isSimulatorRunning,
			}}
		>
			{children}
		</SimulatorContext.Provider>
	);
};

export const useSimulatorApp = (): SimulatorApp => {
	const ctx = React.useContext(SimulatorContext);

	if (!ctx || !ctx.simulatorApp) {
		throw new Error("useSimulatorApp used before simulator was initialized");
	}

	return ctx.simulatorApp;
};

export const useSimulatorAppControls = (): Pick<
	SimulatorContextValue,
	"startSimulator" | "stopSimulator" | "isSimulatorRunning"
> => {
	const ctx = React.useContext(SimulatorContext);

	if (!ctx) {
		throw new Error(
			"useSimulatorAppControls used outside the scope of the provider",
		);
	}

	return ctx;
};
