import * as React from "react";
import { Button } from "../factory";
import styles from "./save-circuit-dialog.module.css";

type SaveCircuitDialogProps = {
  onConfirm: (circuitName: string) => void;
  onDismiss: () => void;
};

export const SaveCircuitDialog: React.FC<SaveCircuitDialogProps> = (props) => {
  const { onDismiss, onConfirm } = props;
  const circuitNameInput = React.useRef<HTMLInputElement>(null);

  return (
    <>
      <div className={styles.circuitNameInput}>
        <input
          data-testid="saveCircuitChipNameInput"
          className={styles.circuitNameInput}
          type="text"
          ref={circuitNameInput}
          autoFocus={true}
          placeholder={`CHIP NAME`}
        />
      </div>
      <div className={styles.actionButtons}>
        <div className={styles.actionButton}>
          <Button
            dataTestId="saveCircuitCancelButton"
            text="CANCEL"
            appearance="dark"
            fullWidth
            size="small"
            onClick={onDismiss}
          />
        </div>
        <div className={styles.actionButton}>
          <Button
            dataTestId="saveCircuitConfirmButton"
            text="SAVE"
            appearance="dark"
            fullWidth
            size="small"
            onClick={() => {
              if (
                circuitNameInput.current &&
                circuitNameInput.current.value.length > 0
              ) {
                onConfirm(circuitNameInput.current.value.toUpperCase());
              }
            }}
          />
        </div>
      </div>
    </>
  );
};
