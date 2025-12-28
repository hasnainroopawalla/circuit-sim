import * as React from "react";

export const useCloseDialogShortcut = (closeDialog: () => void) => {
	React.useEffect(() => {
		const handler = (e: KeyboardEvent) => {
			// if ((e.metaKey || e.ctrlKey) && e.key === "k") {
			// 	e.preventDefault();
			// 	setIsOpen(!open);
			// }
			if (e.key === "Escape") {
				closeDialog();
			}
		};

		window.addEventListener("keydown", handler);

		return () => window.removeEventListener("keydown", handler);
	}, [closeDialog]);
};
