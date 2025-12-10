import type * as React from "react";

type FloatingBarProps = {
	onMenuButtonClick: () => void;
	onSettingsButtonClick: () => void;
};

export const Toolbar = ({
	onMenuButtonClick,
	onSettingsButtonClick,
}: FloatingBarProps) => {
	return (
		<div className="fixed bottom-6 left-1/2 z-50 -translate-x-1/2">
			<div className="flex items-center gap-1 rounded-xl bg-black/40 px-2 py-2 backdrop-blur-md shadow-lg">
				<ToolbarItem text="Menu" onClick={onMenuButtonClick} />
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
