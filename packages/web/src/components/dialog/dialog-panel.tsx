import type * as React from "react";
import { CommandPalette } from "../command-palette";
import { SaveChipDialog, type SaveChipDialogProps } from "../save-chip-dialog";

export type PanelKindProps =
	| ({
			kind: "commandPalette";
	  } & {})
	| ({
			kind: "saveChip";
	  } & SaveChipDialogProps);

export const getCurrentPanelProps = (
	props: PanelKindProps | null,
): {
	Component: React.ReactElement;
} => {
	switch (props?.kind) {
		case "commandPalette":
			return {
				Component: <CommandPalette />,
			};
		case "saveChip":
			return {
				Component: <SaveChipDialog {...props} />,
			};
		default:
			return {
				Component: <></>,
			};
	}
};
