import * as React from "react";
import { SimulatorApp, type ChipSpec } from "@digital-logic-sim/simulator";

export type ISimulatorAppContext = {
	startSimulator: (canvas: HTMLCanvasElement) => void;
	stopSimulator: () => void;
	getChipSpecs: () => ChipSpec[];
};

const SimulatorAppContext = React.createContext<ISimulatorAppContext>(
	{} as ISimulatorAppContext,
);

export const SimulatorAppProvider = ({ children }: React.PropsWithChildren) => {
	const [simulatorApp, setSimulatorApp] = React.useState<SimulatorApp | null>(
		null,
	);

	const startSimulator = React.useCallback(
		(canvas: HTMLCanvasElement) => {
			if (simulatorApp) {
				return;
			}

			const simApp = new SimulatorApp({ canvas });
			setSimulatorApp(simApp);
			simApp.start();
		},
		[simulatorApp],
	);

	const stopSimulator = React.useCallback(() => {
		simulatorApp?.stop();
	}, [simulatorApp]);

	const getChipSpecs = React.useCallback(() => {
		return simulatorApp?.sim.chipLibraryService.getAll() ?? [];
	}, [simulatorApp]);

	return (
		<SimulatorAppContext.Provider
			value={{
				startSimulator,
				stopSimulator,
				getChipSpecs,
			}}
		>
			{children}
		</SimulatorAppContext.Provider>
	);
};

export const useSimulatorApp = () => React.useContext(SimulatorAppContext);
