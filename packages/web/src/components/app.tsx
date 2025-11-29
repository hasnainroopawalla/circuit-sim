import * as React from "react";
import { Canvas } from "./canvas";
import { Toolbar } from "./toolbar";
import {
	SimulatorAppProvider,
	useSimulatorApp,
} from "../contexts/simulator-app-context";

export const App = () => {
	return (
		<SimulatorAppProvider>
			<Sim />
		</SimulatorAppProvider>
	);
};

const Sim: React.FC = () => {
	const canvasRef = React.useRef<HTMLCanvasElement>(null);

	const { startSimulator, stopSimulator } = useSimulatorApp();

	React.useEffect(() => {
		if (!canvasRef.current) {
			return;
		}

		startSimulator(canvasRef.current);

		return () => stopSimulator();
	}, [startSimulator, stopSimulator]);

	return (
		<div id="app-container" className="h-full border-2 border-blue-500">
			<Canvas canvasRef={canvasRef} />
			<Toolbar />
		</div>
	);
};
