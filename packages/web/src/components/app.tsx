import * as React from "react";
import { Canvas } from "./canvas";
import { SimulatorApp } from "@digital-logic-sim/simulator";

export const App: React.FC = () => {
	const canvasRef = React.useRef<HTMLCanvasElement>(null);

	React.useEffect(() => {
		if (!canvasRef.current) {
			return;
		}

		const simulatorApp = new SimulatorApp({ canvas: canvasRef.current });
		simulatorApp.start();
	}, []);

	return (
		<div id="app-container" className="h-full border-4 border-blue-500">
			<Canvas canvasRef={canvasRef} />
		</div>
	);
};
