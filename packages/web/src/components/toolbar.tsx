import * as React from "react";
import { useOnClickOutside } from "../utils";
import { useSimulatorApp } from "../contexts/simulator-app-context";
import { useDialog } from "./dialog";

export const Toolbar: React.FC = () => {
	const { openDialog } = useDialog();

	return (
		<div className="fixed z-50 -translate-x-1/2 bottom-6 left-1/2">
			<div className="relative flex items-center gap-1 px-2 py-2 shadow-lg rounded-xl bg-black/40 backdrop-blur-md">
				<ProjectToolbarItem />
				<ToolbarItem
					text="Actions"
					onClick={() => openDialog({ kind: "commandPalette" })}
				/>
				<ToolbarItem text="Settings" onClick={() => {}} />
			</div>
		</div>
	);
};

type ToolbarItemProps = {
	text: string;
	onClick: () => void;
};

const ToolbarItem: React.FC<ToolbarItemProps> = ({ text, onClick }) => {
	return (
		<button
			onClick={onClick}
			className="cursor-pointer rounded-md px-4 py-1.5 text-sm text-white/80 transition hover:bg-white/10 hover:text-white text-left"
		>
			{text}
		</button>
	);
};

const ProjectToolbarItem: React.FC = () => {
	const { openDialog, closeDialog } = useDialog();

	const simulatorApp = useSimulatorApp();

	const [isPopoverOpen, setIsPopoverOpen] = React.useState(false);
	const divRef = React.useRef<HTMLDivElement>(null);

	useOnClickOutside(divRef, () => setIsPopoverOpen(false));

	const saveChip = React.useCallback(
		(chipName: string) => {
			simulatorApp.sim.emit("sim.save-chip.start", { chipName });
			setIsPopoverOpen(false);
			closeDialog();
		},
		[simulatorApp, closeDialog],
	);

	const importBlueprint = React.useCallback(
		(blueprintString: string) => {
			simulatorApp.sim.emit("sim.import-blueprint.start", { blueprintString });
			setIsPopoverOpen(false);
			closeDialog();
		},
		[simulatorApp, closeDialog],
	);

	const resetSimulator = React.useCallback(() => {
		simulatorApp.sim.emit("sim.reset", undefined);
		setIsPopoverOpen(false);
	}, [simulatorApp]);

	return (
		<div className="relative" ref={divRef}>
			<ToolbarItem text="Project" onClick={() => setIsPopoverOpen((v) => !v)} />

			{isPopoverOpen && (
				<div className="absolute flex flex-col py-2 mb-2 -translate-x-1/2 rounded-lg shadow-lg bottom-full left-1/2 min-w-max bg-neutral-900/70 backdrop-blur-xl ring-1 ring-white/10">
					<ToolbarItem
						text="Save"
						onClick={() => openDialog({ kind: "saveChip", onSave: saveChip })}
					/>
					<ToolbarItem
						text="Import"
						onClick={() =>
							openDialog({ kind: "importBlueprint", onImport: importBlueprint })
						}
					/>
					<ToolbarItem text="Reset" onClick={resetSimulator} />
				</div>
			)}
		</div>
	);
};
