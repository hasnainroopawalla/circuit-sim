import { pubsub } from "@circuit-sim/pubsub";
import type * as React from "react";
import type { IconType } from "react-icons";
import { ImportIcon, SaveIcon } from "../icons";
import { ImportChipDialog } from "./import-chip/import-chip-dialog";
import { SaveChipDialog } from "./save-chip";

export type PanelKind = "saveCircuit" | "importChip";

export const getCurrentPanelProps = (
	kind: PanelKind | null,
	closeDialog: () => void,
): {
	Component: () => React.ReactElement;
	Icon: IconType;
	title: string;
} => {
	switch (kind) {
		case "saveCircuit":
			return {
				Component: () => (
					<SaveChipDialog
						onConfirm={(circuitName: string) => {
							pubsub.publish("SaveCircuit", { name: circuitName });
							closeDialog();
						}}
					/>
				),
				Icon: SaveIcon,
				title: "SAVE CHIP",
			};
		case "importChip":
			return {
				Component: () => (
					<ImportChipDialog
						onConfirm={(chipName: string, blueprint: string) => {
							pubsub.publish("ImportChip", {
								chipName,
								blueprint,
							});
							closeDialog();
						}}
					/>
				),
				Icon: ImportIcon,
				title: "IMPORT CHIP",
			};
		default:
			return {
				Component: () => <></>,
				Icon: () => <></>,
				title: "",
			};
	}
};
