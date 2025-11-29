import type * as React from "react";
import { SimulatorCanvas } from "./canvas";
import { Toolbar } from "./toolbar";
import { SimulatorAppProvider } from "../contexts/simulator-app-context";

export const App: React.FC = () => {
	return (
		<SimulatorAppProvider>
			<div id="app-container" className="h-full border-2 border-blue-500">
				<SimulatorCanvas>
					<Toolbar />
				</SimulatorCanvas>
			</div>
		</SimulatorAppProvider>
	);
};
