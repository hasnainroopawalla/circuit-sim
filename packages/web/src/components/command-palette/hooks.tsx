import * as React from "react";
import { useSimulatorApp } from "../../contexts/simulator-app-context";
import type { Command } from "./command-palette";

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
					action: () => {
						simulatorApp.sim.emit("chip.spawn.start", {
							chipDefinition,
						});
					},
				})),
		[simulatorApp],
	);

	return commands;
};
