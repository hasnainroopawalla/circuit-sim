import * as React from "react";
import { SimulatorCanvas, SimulatorOverlayView } from "./simulator-canvas";
import { Toolbar } from "./toolbar";
import { StartSimulatorAction } from "./start-simulator-action";
import type { SimulatorApp } from "@digital-logic-sim/simulator";
import { SimulatorAppProvider } from "../contexts/simulator-app-context";
import { useStateRef } from "../utils";
import { Dialog, DialogProvider } from "./dialog";
import { WebGpuErrorBanner } from "./webgpu-error-banner";
import { config } from "../config";

const App: React.FC = () => {
	const [simulatorApp, setSimulatorApp] = React.useState<SimulatorApp | null>(
		null,
	);

	const [canvas, setCanvas, canvasRef] = useStateRef<HTMLCanvasElement | null>(
		null,
	);

	const [startSimError, setStartSimError] = React.useState<boolean>(false);

	return startSimError ? (
		<WebGpuErrorBanner />
	) : (
		<>
			<SimulatorCanvas canvasRef={canvasRef} onCanvasReady={setCanvas} />

			{canvas && !simulatorApp && (
				<StartSimulatorAction
					canvas={canvas}
					onSimulatorAppStartSuccess={setSimulatorApp}
					onSimulatorAppStartFailure={() => setStartSimError(true)}
				/>
			)}

			{simulatorApp && (
				<SimulatorAppProvider simulatorApp={simulatorApp}>
					<Toolbar />
					<SimulatorOverlayView />
					<Dialog />
				</SimulatorAppProvider>
			)}
		</>
	);
};

export const ContextualApp: React.FC = () => {
	return (
		<DialogProvider>
			<App />
			<div className="pointer-events-none fixed bottom-0 right-0 flex justify-center p-1">
				<div className="pointer-events-auto rounded bg-black/40 px-3 py-1 text-xs text-gray-300 backdrop-blur">
					<a
						href={config.githubRepoUrl}
						target="_blank"
						rel="noreferrer"
						className="hover:text-white transition-colors"
					>
						GitHub
					</a>
				</div>
			</div>
		</DialogProvider>
	);
};
