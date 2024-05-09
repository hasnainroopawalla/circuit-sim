import * as React from "react";
import type { IconType } from "react-icons";
import { pubsub } from "@circuit-sim/pubsub";
import { LuImport as ImportIcon } from "react-icons/lu";
import { FaSave as SaveIcon } from "react-icons/fa";
import { SaveCircuitDialog } from "./save-circuit-dialog";
import { ImportChipDialog } from "./import-chip-dialog";

export type PanelKind = "saveCircuit" | "importChip";

export const getCurrentPanelProps = (
  kind: PanelKind | null
): {
  component: React.ReactElement;
  Icon: IconType;
  title: string;
} => {
  switch (kind) {
    case "saveCircuit":
      return {
        component: (
          <SaveCircuitDialog
            onConfirm={(circuitName: string) => {
              pubsub.publish("SaveCircuit", { name: circuitName });
            }}
          />
        ),
        Icon: SaveIcon,
        title: "SAVE CIRCUIT",
      };
    case "importChip":
      return {
        component: (
          <ImportChipDialog
            onConfirm={(chipName: string, blueprint: string) => {
              pubsub.publish("ImportChip", {
                chipName,
                blueprint,
              });
            }}
          />
        ),
        Icon: ImportIcon,
        title: "IMPORT CHIP",
      };
    default:
      return {
        component: <></>,
        Icon: () => <></>,
        title: "",
      };
  }
};
