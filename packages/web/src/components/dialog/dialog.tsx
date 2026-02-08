import * as React from "react";
import { useDialog } from "./dialog-context";
import { getCurrentPanelProps } from "./dialog-panel";
import { useOnClickOutside } from "../../utils";

// TODO: Fix height overflow scroll
// TODO: Define headers here
export const Dialog: React.FC = () => {
	const { currentPanel, closeDialog } = useDialog();

	const divRef = React.useRef<HTMLDivElement>(null);
	useOnClickOutside(divRef, closeDialog);

	if (!currentPanel) {
		return null;
	}

	const { title, Component } = getCurrentPanelProps(currentPanel);

	return (
		<div className="fixed inset-0 z-50 flex items-start justify-center pt-24 bg-black/20 backdrop-blur-sm">
			<div
				ref={divRef}
				className="w-full max-w-md shadow-md rounded-xl bg-neutral-900/80 backdrop-blur-xl ring-1 ring-white/10"
			>
				<div className="p-3">
					{title && (
						<h2 className="mb-3 text-lg font-semibold text-white">{title}</h2>
					)}
					{Component}
				</div>
			</div>
		</div>
	);
};
