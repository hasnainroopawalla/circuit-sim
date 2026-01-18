import * as React from "react";
import { useDialog } from "../dialog";
import { useSimulatorApp } from "../../contexts/simulator-app-context";
import type { ChipDefinition } from "@digital-logic-sim/simulator";

export type ChipLibraryDialogProps = {};

export const ChipLibraryDialog: React.FC<ChipLibraryDialogProps> = () => {
	const simulatorApp = useSimulatorApp();

	const { closeDialog, isPanelOpen } = useDialog();

	const [searchString, setSearchString] = React.useState("");

	// clear the search field before rendering
	React.useEffect(() => {
		if (isPanelOpen("chipLibrary")) {
			setSearchString("");
		}
	}, [isPanelOpen]);

	const chips = React.useMemo(
		() => simulatorApp.sim.chipLibraryService.getAllDefinitions(),
		[simulatorApp],
	);

	const filteredChips = React.useMemo(
		() =>
			chips.filter((chip) =>
				chip.name.toLowerCase().includes(searchString.toLowerCase()),
			),
		[searchString, chips],
	);

	const onItemSelect = React.useCallback(
		(chipDefinition: ChipDefinition) => {
			simulatorApp.sim.emit("chip.spawn.start", {
				chipDefinition,
			});
			closeDialog();
		},
		[closeDialog, simulatorApp],
	);

	const onItemView = React.useCallback(
		(chipDefinition: ChipDefinition) => {
			closeDialog();
		},
		[closeDialog],
	);

	return (
		<>
			<input
				autoFocus
				value={searchString}
				onChange={(e) => setSearchString(e.target.value)}
				placeholder="Search chips by name"
				className="w-full px-4 py-3 text-sm text-white border-b outline-none rounded-t-xl border-white/10bg-transparent placeholder:text-white/40"
			/>

			<ul className="py-2 overflow-y-auto max-h-60 select-none">
				{filteredChips.length === 0 && (
					<li className="px-4 py-2 text-sm text-white/40">No results found</li>
				)}

				{filteredChips.map((chip) => (
					<ChipLibraryItem
						key={chip.name}
						defintion={chip}
						onItemSelect={onItemSelect}
						onItemView={onItemView}
					/>
				))}
			</ul>
		</>
	);
};

type ChipLibraryItemProps = {
	defintion: ChipDefinition;
	onItemSelect: (definition: ChipDefinition) => void;
	onItemView: (definition: ChipDefinition) => void;
};

const ChipLibraryItem: React.FC<ChipLibraryItemProps> = ({
	defintion,
	onItemSelect,
	onItemView,
}) => {
	const onSelect = React.useCallback(() => {
		onItemSelect(defintion);
	}, [defintion, onItemSelect]);

	const onView = React.useCallback(
		(e: React.MouseEvent) => {
			e.stopPropagation();
			onItemView(defintion);
		},
		[defintion, onItemView],
	);

	return (
		<li
			className="flex items-center justify-between px-4 py-2 text-sm cursor-pointer text-white/90 hover:bg-white/10"
			onClick={onSelect}
		>
			<span className="flex-1">{defintion.name}</span>

			<div className="flex gap-2">
				<button
					onClick={onView}
					className="cursor-pointer px-2 py-1 text-xs bg-gray-700 rounded hover:bg-gray-600"
				>
					View
				</button>
			</div>
		</li>
	);
};
