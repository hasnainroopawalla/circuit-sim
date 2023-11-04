import * as React from "react";
import { EmitterEvent, emitter } from "../../event-service";
import { useCustomChips } from "../hooks";
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
        useCustomChips={useCustomChips}
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
                emitter.emit(EmitterEvent.SaveCircuit, { name: circuitName });
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
              onConfirm={(customChipName: string, blueprint: string) => {
                emitter.emit(EmitterEvent.ImportCustomChip, {
                  customChipName,
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
