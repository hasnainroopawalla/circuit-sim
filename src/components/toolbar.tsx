import React from "react";
import { EmitterEvent, emitter } from "../event-service";
import { useCustomChips } from "./hooks";
import { ImportChipDialog, SaveCircuitDialog } from "./dialogs";
import { Button, Dialog } from "./factory";
import { LuImport } from "react-icons/lu";
import { FaSave } from "react-icons/fa";

import styles from "./toolbar.module.css";

export const Toolbar = () => {
  const customChips = useCustomChips();

  const [showSaveCircuitDialog, setSaveShowCircuitDialog] =
    React.useState(false);

  const [showImportChipDialog, setShowImportChipDialog] = React.useState(false);

  return (
    <>
      <div className={`${styles.toolbar} ${styles.upperToolbar}`}>
        <Button
          text="SAVE"
          appearance="primary"
          size="large"
          onClick={() => setSaveShowCircuitDialog(true)}
        />
        <Button
          text="OPTIONS"
          appearance="secondary"
          size="large"
          onClick={() => {}}
        />
      </div>
      <div className={`${styles.toolbar} ${styles.lowerToolbar}`}>
        <Button
          text="AND"
          appearance="secondary"
          size="large"
          onClick={() =>
            emitter.emit(EmitterEvent.SpawnCoreChip, {
              coreChip: "AND",
            })
          }
        />
        <Button
          text="OR"
          appearance="secondary"
          size="large"
          onClick={() =>
            emitter.emit(EmitterEvent.SpawnCoreChip, {
              coreChip: "OR",
            })
          }
        />
        <Button
          text="NOT"
          appearance="secondary"
          size="large"
          onClick={() =>
            emitter.emit(EmitterEvent.SpawnCoreChip, {
              coreChip: "NOT",
            })
          }
        />
        {customChips.map((customChip) => (
          <Button
            key={customChip.name}
            text={customChip.name}
            appearance="secondary"
            size="large"
            onClick={customChip.onClick}
          />
        ))}
        <Button
          text="+"
          appearance="primary"
          size="large"
          onClick={() => setShowImportChipDialog(true)}
        />
      </div>
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
              onConfirm={(blueprint: string) => {
                emitter.emit(EmitterEvent.ImportCustomChip, {
                  blueprint: blueprint,
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
