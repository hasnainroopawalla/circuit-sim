import { Simulator } from "@circuit-sim/simulator";
import * as React from "react";

export const SimulatorContext = React.createContext<Simulator>({} as Simulator);

export const SimulatorProvider: React.FC<React.PropsWithChildren> = ({
	children,
}) => {
	const simulator = React.useMemo(() => new Simulator(), []);

	return (
		<SimulatorContext.Provider value={simulator}>
			{children}
		</SimulatorContext.Provider>
	);
};

export const useSimulator = () => React.useContext(SimulatorContext);
