import React from "react";
import { Button } from "../factory";
import styles from "./import-chip-dialog.module.css";

type ImportChipDialog = {
  onConfirm: (circuitName: string) => void;
  onDismiss: () => void;
};

export const ImportChipDialog: React.FC<ImportChipDialog> = (props) => {
  const { onDismiss, onConfirm } = props;
  const blueprintInput = React.useRef<HTMLInputElement>(null);

  return (
    <>
      <div className={styles.circuitNameInput}>
        <input
          className={styles.circuitNameInput}
          type="text"
          ref={blueprintInput}
          autoFocus={true}
          placeholder={`e.g. {"name":"NAND"..`}
          size={40}
        />
      </div>
      <div className={styles.actionButtons}>
        <div className={styles.actionButton}>
          <Button
            text="CANCEL"
            appearance="dark"
            fullWidth
            size="small"
            onClick={onDismiss}
          />
        </div>
        <div className={styles.actionButton}>
          <Button
            text="IMPORT"
            appearance="dark"
            fullWidth
            size="small"
            onClick={() => {
              if (
                blueprintInput.current &&
                blueprintInput.current.value.length > 0
              ) {
                onConfirm(blueprintInput.current.value);
              }
            }}
          />
        </div>
      </div>
    </>
  );
};
