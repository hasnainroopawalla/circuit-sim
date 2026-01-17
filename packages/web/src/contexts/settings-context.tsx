import * as React from "react";
import type { Settings } from "@digital-logic-sim/shared-types";
import { useSimulatorApp } from "./simulator-app-context";

type SettingsContextValue = {
	getSettings: () => Settings;
	updateSettings: (newSettings: Settings) => void;
};

const SettingsContext = React.createContext<SettingsContextValue>({
	getSettings: () => ({}) as Settings,
	updateSettings: () => {},
});

export const SettingsProvider = ({ children }: React.PropsWithChildren) => {
	const simulatorApp = useSimulatorApp();

	const updateSettings = React.useCallback(
		(newSettings: Settings) => {
			simulatorApp.settingsService.set(newSettings);
		},
		[simulatorApp],
	);

	const getSettings = React.useCallback(
		() => simulatorApp.settingsService.get(),
		[simulatorApp],
	);

	return (
		<SettingsContext.Provider
			value={{
				getSettings,
				updateSettings,
			}}
		>
			{children}
		</SettingsContext.Provider>
	);
};

export const useSettings = (): SettingsContextValue =>
	React.useContext(SettingsContext);
