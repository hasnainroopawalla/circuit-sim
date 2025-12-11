import * as React from "react";
import { useOnClickOutside } from "../utils";
import { useSimulatorApp } from "../contexts/simulator-app-context";

type ToolbarProps = {
	onMenuButtonClick: () => void;
	onSettingsButtonClick: () => void;
};

export const Toolbar = ({
	onMenuButtonClick,
	onSettingsButtonClick,
}: ToolbarProps) => {
	return (
		<div className="fixed bottom-6 left-1/2 z-50 -translate-x-1/2">
			<div className="flex items-center gap-1 rounded-xl bg-black/40 px-2 py-2 backdrop-blur-md shadow-lg relative">
				<ProjectToolbarItem onNewChipClick={() => {}} />
				<ToolbarItem text="Actions" onClick={onMenuButtonClick} />
				<ToolbarItem text="Settings" onClick={onSettingsButtonClick} />
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
			className="cursor-pointer rounded-md px-4 py-1.5 text-sm text-white/80 transition hover:bg-white/10 hover:text-white"
		>
			{text}
		</button>
	);
};

type ProjectToolbarItemProps = {
	onNewChipClick: () => void;
};

const ProjectToolbarItem: React.FC<ProjectToolbarItemProps> = ({
	onNewChipClick,
}) => {
	const simulatorApp = useSimulatorApp();

	const [isPopoverOpen, setIsPopoverOpen] = React.useState(false);
	const divRef = React.useRef<HTMLDivElement>(null);

	useOnClickOutside(divRef, () => setIsPopoverOpen(false));

	const onSaveChipClick = React.useCallback(() => {
		simulatorApp.sim.emit("chip.save", undefined);
		setIsPopoverOpen(false);
	}, [simulatorApp]);

	return (
		<div className="relative" ref={divRef}>
			<ToolbarItem text="Project" onClick={() => setIsPopoverOpen((v) => !v)} />

			{isPopoverOpen && (
				<div className="absolute bottom-full left-1/2 mb-2 min-w-max -translate-x-1/2 rounded-lg bg-neutral-900/70 backdrop-blur-xl shadow-lg ring-1 ring-white/10 py-2 flex flex-col">
					<PopoverItem
						text="New Chip"
						onClick={() => {
							setIsPopoverOpen(false);
							onNewChipClick();
						}}
					/>
					<PopoverItem text="Save Chip" onClick={onSaveChipClick} />
				</div>
			)}
		</div>
	);
};

const PopoverItem: React.FC<ToolbarItemProps> = (props) => {
	return <ToolbarItem {...props} />;
};
