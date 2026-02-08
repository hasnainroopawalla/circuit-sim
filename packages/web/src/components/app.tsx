import * as React from "react";
import { SimulatorCanvas, SimulatorOverlayView } from "./simulator-canvas";
import { Toolbar } from "./toolbar";
import { StartSimulatorAction } from "./start-simulator-action";
import type { SimulatorApp } from "@digital-logic-sim/simulator";
import { SimulatorAppProvider } from "../contexts/simulator-app-context";
import { useStateRef } from "../utils";
import { Dialog, DialogProvider } from "./dialog";
import { Popover } from "./popover";
import { WebGpuErrorBanner } from "./webgpu-error-banner";
import { Footer } from "./footer";
import { SettingsProvider } from "../contexts/settings-context";

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
					<SettingsProvider>
						<Toolbar />
						<SimulatorOverlayView />
						<Dialog />
						<Popover />
					</SettingsProvider>
				</SimulatorAppProvider>
			)}
		</>
	);
};

export const ContextualApp: React.FC = () => {
	return (
		<DialogProvider>
			<App />
			<Footer />
		</DialogProvider>
	);
};
