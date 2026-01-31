import * as React from "react";
import type { PanelKindProps } from "./dialog-panel";

type IDialogContext = {
	openDialog: (kind: PanelKindProps) => void;
	closeDialog: () => void;
	currentPanel: PanelKindProps | null;
	isPanelOpen: (kind: PanelKindProps["kind"]) => boolean;
};

const DialogContext = React.createContext<IDialogContext>({} as IDialogContext);

export const DialogProvider = (props: React.PropsWithChildren) => {
	const [currentPanel, setCurrentPanel] = React.useState<PanelKindProps | null>(
		null,
	);

	const openDialog = React.useCallback((props: PanelKindProps) => {
		setCurrentPanel(props);
	}, []);

	const closeDialog = React.useCallback(() => {
		setCurrentPanel(null);
	}, []);

	const isPanelOpen = React.useCallback(
		(kind: PanelKindProps["kind"]) => {
			return currentPanel?.kind === kind;
		},
		[currentPanel],
	);

	return (
		<DialogContext.Provider
			value={{
				openDialog,
				closeDialog,
				currentPanel,
				isPanelOpen,
			}}
		>
			{props.children}
		</DialogContext.Provider>
	);
};

export const useDialog = () => React.useContext(DialogContext);
