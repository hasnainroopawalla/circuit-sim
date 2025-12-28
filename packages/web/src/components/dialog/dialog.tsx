import type * as React from "react";
import { useDialog } from "./dialog-context";
import { getCurrentPanelProps } from "./dialog-panel";
import { useCloseDialogShortcut } from "./hooks";

export const Dialog: React.FC = () => {
	const { currentPanel, closeDialog } = useDialog();

	useCloseDialogShortcut(closeDialog);

	if (!currentPanel) {
		return null;
	}

	const { Component } = getCurrentPanelProps(currentPanel);

	return (
		<div className="fixed inset-0 z-50 flex items-start justify-center pt-24 bg-black/20 backdrop-blur-sm">
			<div className="w-full max-w-md shadow-md rounded-xl bg-neutral-900/80 backdrop-blur-xl ring-1 ring-white/10">
				{Component}
			</div>
		</div>
	);
};
