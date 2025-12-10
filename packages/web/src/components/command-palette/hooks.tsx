import * as React from "react";
import { useSimulatorApp } from "../../contexts/simulator-app-context";
import type { CommandPaletteProps, Command } from "./command-palette";

export const useShortcut = ({ open, setIsOpen }: CommandPaletteProps) => {
	React.useEffect(() => {
		const handler = (e: KeyboardEvent) => {
			if ((e.metaKey || e.ctrlKey) && e.key === "k") {
				e.preventDefault();
				setIsOpen(!open);
			}
			if (e.key === "Escape") {
				setIsOpen(false);
			}
		};

		window.addEventListener("keydown", handler);

		return () => window.removeEventListener("keydown", handler);
	}, [open, setIsOpen]);
};

export const useCommands = (): Command[] => {
	const simulatorApp = useSimulatorApp();

	// TODO: Split based on commands (spawn, delete, etc.)
	const commands = React.useMemo(
		() =>
			simulatorApp.sim.chipLibraryService.getAll().map((chipSpec) => ({
				id: chipSpec.name,
				label: `Spawn: ${chipSpec.name}`,
				action: () => simulatorApp.sim.emit("chip.spawn", chipSpec),
			})),
		[simulatorApp],
	);

	return commands;
};
