import * as React from "react";
import { emitter } from "@circuit-sim/events";
import { useCircuitChips } from "../hooks";
import { ImportChipDialog, SaveCircuitDialog } from "../dialogs";
import { Dialog } from "../factory";
import { LuImport } from "react-icons/lu";
import { FaSave } from "react-icons/fa";

import { LowerToolbar } from "./lower-toolbar";

export const Toolbar = () => {
  const [showSaveCircuitDialog, setSaveShowCircuitDialog] =
    React.useState(false);

  const [showImportChipDialog, setShowImportChipDialog] = React.useState(false);

  return (
    <>
      <LowerToolbar
        useCircuitChips={useCircuitChips}
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
          icon={FaSave}
          content={
            <SaveCircuitDialog
              onConfirm={(circuitName: string) => {
                emitter.emit("SaveCircuit", { name: circuitName });
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
          icon={LuImport}
          content={
            <ImportChipDialog
              onConfirm={(chipName: string, blueprint: string) => {
                emitter.emit("ImportChip", {
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
