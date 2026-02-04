import type * as React from "react";

type ActionMenuProps = {
	onView: () => void;
	onDelete: () => void;
};

export const ActionMenu: React.FC<ActionMenuProps> = ({ onView, onDelete }) => {
	return (
		<div className="py-1 text-sm text-white">
			<button
				type="button"
				onClick={onView}
				className="flex items-center w-full gap-2 px-3 py-2 cursor-pointer hover:bg-white/5"
			>
				View
			</button>

			<button
				type="button"
				onClick={onDelete}
				className="flex items-center w-full gap-2 px-3 py-2 cursor-pointer text-rose-400 hover:bg-white/5"
			>
				Delete
			</button>
		</div>
	);
};

export default ActionMenu;
