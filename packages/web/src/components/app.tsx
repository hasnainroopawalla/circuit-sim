import  * as React from "react";
import { Canvas } from "./canvas";
import { runSimulator } from "@digital-logic-sim/simulator";

export const App: React.FC = () => {
	React.useEffect(() => {
		runSimulator();
	}, []);

	return <div id="app-container"><span>Hello</span><Canvas /></div>;
};
