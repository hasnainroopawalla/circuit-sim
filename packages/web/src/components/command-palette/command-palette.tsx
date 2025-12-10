import * as React from "react";
import { useShortcut, useCommands } from "./hooks";

export type Command = {
	id: string;
	label: string;
	action: () => void;
};

export type CommandPaletteProps = {
	isOpen: boolean;
	onClose: () => void;
};

export const CommandPalette = (props: CommandPaletteProps) => {
	const { isOpen, onClose } = props;

	const [query, setQuery] = React.useState("");

	// clear the search field before rendering
	React.useEffect(() => {
		if (isOpen) {
			setQuery("");
		}
	}, [isOpen]);

	useShortcut(props);

	const commands = useCommands();

	const filtered = React.useMemo(
		() =>
			commands.filter((cmd) =>
				cmd.label.toLowerCase().includes(query.toLowerCase()),
			),
		[query, commands],
	);

	if (!isOpen) {
		return null;
	}

	return (
		<div className="fixed inset-0 z-50 flex items-start justify-center bg-black/20 backdrop-blur-sm pt-24">
			<div className="w-full max-w-md rounded-xl bg-neutral-900/80 backdrop-blur-xl shadow-md ring-1 ring-white/10">
				<input
					autoFocus
					value={query}
					onChange={(e) => setQuery(e.target.value)}
					placeholder="Type a command…"
					className="w-full rounded-t-xl border-b border-white/10bg-transparent px-4 py-3 text-sm text-white placeholder:text-white/40 outline-none"
				/>

				<ul className="max-h-60 overflow-y-auto py-2">
					{filtered.length === 0 && (
						<li className="px-4 py-2 text-sm text-white/40">
							No results found
						</li>
					)}

					{filtered.map((command) => (
						<CommandItem key={command.id} command={command} onClick={onClose} />
					))}
				</ul>
			</div>
		</div>
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
			className="cursor-pointer px-4 py-2 text-sm text-white/90 hover:bg-white/10"
		>
			{command.label}
		</li>
	);
};
