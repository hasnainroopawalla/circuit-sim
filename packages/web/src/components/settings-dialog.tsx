import * as React from "react";
import { useDialog } from "./dialog";
import { useSettings } from "../contexts/settings-context";
import type { Settings } from "@digital-logic-sim/shared-types";
import { twMerge } from "tailwind-merge";

export type SettingsDialogProps = {};

export const SettingsDialog: React.FC<SettingsDialogProps> = () => {
	const { closeDialog } = useDialog();

	const { getSettings, updateSettings } = useSettings();

	const [settings, setSettings] = React.useState<Settings>(() => getSettings());

	const toggleSetting = React.useCallback((key: keyof Settings) => {
		setSettings((prev) => ({ ...prev, [key]: !prev[key] }));
	}, []);

	const onSave = React.useCallback(() => {
		updateSettings(settings);
		closeDialog();
	}, [settings, updateSettings, closeDialog]);

	return (
		<div className="flex flex-col gap-4">
			<div className="space-y-4">
				<SettingsSection title="Visualization">
					<SettingRow
						label="Show grid"
						value={settings.showGrid}
						onToggle={() => toggleSetting("showGrid")}
					/>
				</SettingsSection>
			</div>

			<div className="flex justify-end gap-2">
				<button
					onClick={closeDialog}
					className="cursor-pointer rounded px-3 py-1.5 text-sm text-gray-300 hover:bg-gray-700"
				>
					Cancel
				</button>
				<button
					onClick={onSave}
					className="cursor-pointer rounded bg-blue-600 px-3 py-1.5 text-sm text-white hover:bg-blue-500"
				>
					Save
				</button>
			</div>
		</div>
	);
};

const SettingsSection: React.FC<{
	title: string;
	children: React.ReactNode;
}> = ({ title, children }) => (
	<div>
		<div className="mb-2 text-xs tracking-wide text-gray-400 uppercase">
			{title}
		</div>
		<div className="space-y-2">{children}</div>
	</div>
);

const SettingRow: React.FC<{
	label: string;
	value: boolean;
	onToggle: () => void;
}> = ({ label, value, onToggle }) => {
	return (
		<div
			onClick={onToggle}
			className="flex items-center justify-between rounded px-2 py-1.5 hover:bg-white/5"
		>
			<span className="text-sm text-gray-200">{label}</span>

			<div
				className={twMerge(
					"h-5 w-9 rounded-full transition-colors cursor-pointer",
					value ? "bg-blue-600" : "bg-gray-600",
				)}
			>
				<div
					className={twMerge(
						"h-5 w-5 rounded-full bg-white transition-transform",
						value ? "translate-x-4" : "translate-x-0",
					)}
				/>
			</div>
		</div>
	);
};
