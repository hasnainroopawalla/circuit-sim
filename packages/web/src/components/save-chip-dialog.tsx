import * as React from "react";
import { useDialog } from "./dialog";

export type SaveChipDialogProps = {
	onSave: (name: string) => void;
};

export const SaveChipDialog: React.FC<SaveChipDialogProps> = ({ onSave }) => {
	const { closeDialog } = useDialog();

	const [name, setName] = React.useState("");

	const handleSave = React.useCallback(() => {
		if (!name) {
			return;
		}

		const formattedName = name.trim();
		onSave(formattedName);
	}, [name, onSave]);

	return (
		<div>
			<input
				autoFocus
				type="text"
				value={name}
				onChange={(e) => setName(e.target.value)}
				placeholder="Chip name"
				className="w-full px-3 py-2 mb-4 text-lg text-white border border-gray-600 rounded outline-none bg-neutral-800 focus:border-blue-500"
			/>

			<div className="flex justify-end gap-2">
				<button
					onClick={closeDialog}
					className="cursor-pointer rounded px-3 py-1.5 text-sm text-gray-300 hover:bg-gray-700"
				>
					Cancel
				</button>

				<button
					onClick={handleSave}
					disabled={!name}
					className="cursor-pointer rounded bg-blue-600 px-3 py-1.5 text-sm text-white hover:bg-blue-500 disabled:opacity-30 disabled:cursor-not-allowed"
				>
					Save
				</button>
			</div>
		</div>
	);
};
