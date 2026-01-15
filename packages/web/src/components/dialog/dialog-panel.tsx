import type * as React from "react";
import { CommandPalette } from "../command-palette";
import { SaveChipDialog, type SaveChipDialogProps } from "../save-chip-dialog";
import {
	ImportBlueprintDialog,
	type ImportBlueprintDialogProps,
} from "../simulator-canvas/import-blueprint-dialog/import-blueprint-dialog";

export type PanelKindProps =
	| ({
			kind: "commandPalette";
	  } & {})
	| ({
			kind: "saveChip";
	  } & SaveChipDialogProps)
	| ({
			kind: "importBlueprint";
	  } & ImportBlueprintDialogProps);

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
		case "importBlueprint":
			return {
				Component: <ImportBlueprintDialog {...props} />,
			};
		default:
			return {
				Component: <></>,
			};
	}
};
