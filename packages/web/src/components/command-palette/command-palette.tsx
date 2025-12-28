import * as React from "react";
import { useCommands } from "./hooks";
import { useDialog } from "../dialog";

export type Command = {
	id: string;
	label: string;
	action: () => void;
};

export type CommandPaletteProps = {};

export const CommandPalette: React.FC<CommandPaletteProps> = () => {
	const { closeDialog, isPanelOpen } = useDialog();

	const [query, setQuery] = React.useState("");

	// clear the search field before rendering
	React.useEffect(() => {
		if (isPanelOpen("commandPalette")) {
			setQuery("");
		}
	}, [isPanelOpen]);

	const commands = useCommands();

	const filtered = React.useMemo(
		() =>
			commands.filter((cmd) =>
				cmd.label.toLowerCase().includes(query.toLowerCase()),
			),
		[query, commands],
	);

	return (
		<>
			<input
				autoFocus
				value={query}
				onChange={(e) => setQuery(e.target.value)}
				placeholder="Type a command…"
				className="w-full px-4 py-3 text-sm text-white border-b outline-none rounded-t-xl border-white/10bg-transparent placeholder:text-white/40"
			/>

			<ul className="py-2 overflow-y-auto max-h-60">
				{filtered.length === 0 && (
					<li className="px-4 py-2 text-sm text-white/40">No results found</li>
				)}

				{filtered.map((command) => (
					<CommandItem
						key={command.id}
						command={command}
						onClick={closeDialog}
					/>
				))}
			</ul>
		</>
	);
};

type CommandItemProps = {
	onClick: () => void;
	command: Command;
};

const CommandItem: React.FC<CommandItemProps> = ({ command, onClick }) => {
	const onItemClick = React.useCallback(() => {
		command.action();
		onClick();
	}, [command, onClick]);

	return (
		<li
			key={command.id}
			onClick={onItemClick}
			className="px-4 py-2 text-sm cursor-pointer text-white/90 hover:bg-white/10"
		>
			{command.label}
		</li>
	);
};
