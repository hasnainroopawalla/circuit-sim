import * as React from "react";
import { SimulatorCanvas, SimulatorOverlayView } from "./simulator-canvas";
import { Toolbar } from "./toolbar";
import { StartSimulatorAction } from "./start-simulator-action";
import type { SimulatorApp } from "@digital-logic-sim/simulator";
import { SimulatorAppProvider } from "../contexts/simulator-app-context";
import { useStateRef } from "../utils";
import { CommandPalette } from "./command-palette";

export const App: React.FC = () => {
	const [simulatorApp, setSimulatorApp] = React.useState<SimulatorApp | null>(
		null,
	);

	const [canvas, setCanvas, canvasRef] = useStateRef<HTMLCanvasElement | null>(
		null,
	);

	const [isCommandPaletteOpen, setIsCommandPaletteOpen] = React.useState(false);

	return (
		<>
			<SimulatorCanvas canvasRef={canvasRef} onCanvasReady={setCanvas} />

			{canvas && !simulatorApp && (
				<StartSimulatorAction
					canvas={canvas}
					onSimulatorAppStart={setSimulatorApp}
				/>
			)}

			{simulatorApp && (
				<SimulatorAppProvider simulatorApp={simulatorApp}>
					<Toolbar
						onMenuButtonClick={() => setIsCommandPaletteOpen(true)}
						onSettingsButtonClick={() => {}}
					/>
					<CommandPalette
						open={isCommandPaletteOpen}
						setIsOpen={setIsCommandPaletteOpen}
					/>
					<SimulatorOverlayView />
				</SimulatorAppProvider>
			)}
		</>
	);
};
