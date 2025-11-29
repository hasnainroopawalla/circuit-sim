import * as React from "react";
import { useSimulatorAppControls } from "../contexts/simulator-app-context";

const CANVAS_ID = "simulator-canvas"; // TODO: move to config

export const SimulatorCanvas = ({ children }: React.PropsWithChildren) => {
	// const ctx = React.useContext(SimulatorContext);
	const canvasRef = React.useRef<HTMLCanvasElement>(null);

	// if (!ctx) {
	// 	throw new Error("SimulatorCanvas must be inside SimulatorAppProvider");
	// }

	// const { startSimulator, simulatorApp } = ctx;

	const { startSimulator, stopSimulator, isSimulatorRunning } =
		useSimulatorAppControls();

	// Start the simulator only after the canvas has mounted
	React.useLayoutEffect(() => {
		if (!canvasRef.current) {
			return;
		}

		startSimulator(canvasRef.current);

		return () => stopSimulator();
	}, [startSimulator, stopSimulator]);

	return (
		<>
			<canvas className="h-full w-full" id={CANVAS_ID} ref={canvasRef} />
			{isSimulatorRunning && children}
		</>
	);
};
