import * as React from "react";
import { Canvas } from "./canvas";
import { runSimulator } from "@digital-logic-sim/simulator";

export const App: React.FC = () => {
	React.useEffect(() => {
		runSimulator();
	}, []);

	return (
		<div id="app-container" className="border-4 border-blue-500">
			<span>Hello</span>
			<Canvas />
		</div>
	);
};
