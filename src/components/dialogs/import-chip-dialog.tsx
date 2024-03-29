import * as React from "react";
import { Button } from "../factory";
import styles from "./import-chip-dialog.module.css";

type ImportChipDialogProps = {
  onConfirm: (customChipName: string, blueprint: string) => void;
  onDismiss: () => void;
};

export const ImportChipDialog: React.FC<ImportChipDialogProps> = (props) => {
  const { onDismiss, onConfirm } = props;
  const chipNameInput = React.useRef<HTMLInputElement>(null);
  const blueprintInput = React.useRef<HTMLInputElement>(null);

  return (
    <>
      <div className={styles.inputsContainer}>
        <input
          className={styles.chipNameInput}
          data-testid="importChipNameInput"
          type="text"
          ref={chipNameInput}
          autoFocus={true}
          placeholder={`CHIP NAME`}
        />
        <input
          className={styles.blueprintInput}
          type="text"
          data-testid="importChipBlueprintInput"
          ref={blueprintInput}
          autoFocus={true}
          placeholder={`BLUEPRINT`}
          size={40}
        />
      </div>
      <div className={styles.actionButtons}>
        <div className={styles.actionButton}>
          <Button
            dataTestId="importChipCancelButton"
            text="CANCEL"
            appearance="dark"
            fullWidth
            size="small"
            onClick={onDismiss}
          />
        </div>
        <div className={styles.actionButton}>
          <Button
            dataTestId="importChipConfirmButton"
            text="IMPORT"
            appearance="dark"
            fullWidth
            size="small"
            onClick={() => {
              if (
                chipNameInput.current &&
                chipNameInput.current.value.length > 0 &&
                blueprintInput.current &&
                blueprintInput.current.value.length > 0
              ) {
                onConfirm(
                  chipNameInput.current.value,
                  blueprintInput.current.value
                );
              }
            }}
          />
        </div>
      </div>
    </>
  );
};
