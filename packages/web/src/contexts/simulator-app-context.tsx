import * as React from "react";
import type { SimulatorApp } from "@digital-logic-sim/simulator";

type SimulatorAppContextValue = {
	simulatorApp: SimulatorApp;
};

type SimulatorAppProviderProps = SimulatorAppContextValue;

const SimulatorAppContext = React.createContext<SimulatorAppContextValue>(
	{} as SimulatorAppContextValue,
);

export const SimulatorAppProvider = ({
	children,
	simulatorApp,
}: React.PropsWithChildren<SimulatorAppProviderProps>) => {
	return (
		<SimulatorAppContext.Provider
			value={{
				simulatorApp,
			}}
		>
			{children}
		</SimulatorAppContext.Provider>
	);
};

export const useSimulatorApp = (): SimulatorApp =>
	React.useContext(SimulatorAppContext).simulatorApp;
