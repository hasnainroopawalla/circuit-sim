import type * as React from "react";
import {
	ChipLibraryDialog,
	type ChipLibraryDialogProps,
} from "../chip-library-dialog";
import { SaveChipDialog, type SaveChipDialogProps } from "../save-chip-dialog";
import {
	ImportBlueprintDialog,
	type ImportBlueprintDialogProps,
} from "../simulator-canvas/import-blueprint-dialog/import-blueprint-dialog";
import { SettingsDialog, type SettingsDialogProps } from "../settings-dialog";

export type PanelKindProps =
	| ({
			kind: "chipLibrary";
	  } & ChipLibraryDialogProps)
	| ({
			kind: "saveChip";
	  } & SaveChipDialogProps)
	| ({
			kind: "importBlueprint";
	  } & ImportBlueprintDialogProps)
	| ({
			kind: "settings";
	  } & SettingsDialogProps);

export const getCurrentPanelProps = (
	props: PanelKindProps | null,
): {
	title?: string;
	Component: React.ReactElement;
} => {
	switch (props?.kind) {
		case "chipLibrary":
			return {
				Component: <ChipLibraryDialog />,
			};
		case "saveChip":
			return {
				title: "Save Chip",
				Component: <SaveChipDialog {...props} />,
			};
		case "importBlueprint":
			return {
				title: "Import Blueprint",
				Component: <ImportBlueprintDialog {...props} />,
			};
		case "settings":
			return {
				title: "Settings",
				Component: <SettingsDialog />,
			};
		default:
			return {
				Component: <></>,
			};
	}
};
