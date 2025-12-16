import * as React from "react";
import { useSimulatorApp } from "../../contexts/simulator-app-context";
import type { CommandPaletteProps, Command } from "./command-palette";

export const useShortcut = ({ onClose }: CommandPaletteProps) => {
	React.useEffect(() => {
		const handler = (e: KeyboardEvent) => {
			// if ((e.metaKey || e.ctrlKey) && e.key === "k") {
			// 	e.preventDefault();
			// 	setIsOpen(!open);
			// }
			if (e.key === "Escape") {
				onClose();
			}
		};

		window.addEventListener("keydown", handler);

		return () => window.removeEventListener("keydown", handler);
	}, [onClose]);
};

export const useCommands = (): Command[] => {
	const simulatorApp = useSimulatorApp();

	// TODO: Split based on commands (spawn, delete, etc.)
	const commands = React.useMemo(
		() =>
			simulatorApp.sim.chipLibraryService
				.getAllDefinitions()
				.map((chipDefinition) => ({
					id: chipDefinition.name,
					label: `Spawn: ${chipDefinition.name}`,
					action: () =>
						simulatorApp.sim.emit("chip.spawn.start", {
							chipDefinition,
						}),
				})),
		[simulatorApp],
	);

	return commands;
};
