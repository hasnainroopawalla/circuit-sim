import * as React from "react";
import { pubsub } from "@circuit-sim/pubsub";
import { ImportChipDialog, SaveCircuitDialog } from "../dialogs";
import { Dialog } from "../factory";
import { LuImport as ImportIcon } from "react-icons/lu";
import { FaSave as SaveIcon } from "react-icons/fa";
import { LowerToolbar } from "./lower-toolbar";
import { MenuPanel } from "./menu-panel";

export const Toolbar = () => {
  const [showSaveCircuitDialog, setSaveShowCircuitDialog] =
    React.useState(false);

  const [showImportChipDialog, setShowImportChipDialog] = React.useState(false);

  return (
    <>
      <MenuPanel />
      <LowerToolbar
        saveButtonOnClick={() =>
          setSaveShowCircuitDialog(!showSaveCircuitDialog)
        }
        importChipButtonOnClick={() =>
          setShowImportChipDialog(!showImportChipDialog)
        }
      />
      {showSaveCircuitDialog && (
        <Dialog
          title="SAVE CIRCUIT"
          icon={SaveIcon}
          content={
            <SaveCircuitDialog
              onConfirm={(circuitName: string) => {
                pubsub.publish("SaveCircuit", { name: circuitName });
                setSaveShowCircuitDialog(false);
              }}
              onDismiss={() => {
                setSaveShowCircuitDialog(false);
              }}
            />
          }
        />
      )}
      {showImportChipDialog && (
        <Dialog
          title="IMPORT CHIP"
          icon={ImportIcon}
          content={
            <ImportChipDialog
              onConfirm={(chipName: string, blueprint: string) => {
                pubsub.publish("ImportChip", {
                  chipName,
                  blueprint,
                });
                setShowImportChipDialog(false);
              }}
              onDismiss={() => {
                setShowImportChipDialog(false);
              }}
            />
          }
        />
      )}
    </>
  );
};
