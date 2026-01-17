import type * as React from "react";
import { useDialog } from "./dialog";

type SettingsDialogProps = {
	showGrid: boolean;
	setShowGrid: (v: boolean) => void;

	showPinLabels: boolean;
	setShowPinLabels: (v: boolean) => void;

	snapToGrid: boolean;
	setSnapToGrid: (v: boolean) => void;
};

export const SettingsDialog: React.FC<SettingsDialogProps> = ({
	showGrid,
	setShowGrid,
	showPinLabels,
	setShowPinLabels,
	snapToGrid,
	setSnapToGrid,
}) => {
	const { closeDialog } = useDialog();
	return (
		<div className="flex flex-col gap-3">
			<div className="space-y-4">
				<SettingsSection title="Visualization">
					<SettingRow
						label="Show grid"
						value={showGrid}
						onToggle={() => setShowGrid(!showGrid)}
					/>

					<SettingRow
						label="Show pin labels"
						value={showPinLabels}
						onToggle={() => setShowPinLabels(!showPinLabels)}
					/>
				</SettingsSection>
			</div>

			<div className="flex justify-end gap-2">
				<button
					onClick={closeDialog}
					className="cursor-pointer rounded px-3 py-1.5 text-sm text-gray-300 hover:bg-gray-700"
				>
					Confirm
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
			className="flex cursor-pointer items-center justify-between rounded px-2 py-1.5 hover:bg-white/5"
		>
			<span className="text-sm text-gray-200">{label}</span>

			<div
				className={`h-5 w-9 rounded-full transition-colors ${
					value ? "bg-blue-600" : "bg-gray-600"
				}`}
			>
				<div
					className={`h-5 w-5 rounded-full bg-white transition-transform ${
						value ? "translate-x-4" : "translate-x-0"
					}`}
				/>
			</div>
		</div>
	);
};
