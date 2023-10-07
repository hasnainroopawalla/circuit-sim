import React from "react";
import { Button } from "./button";
import styles from "./save-circuit-dialog.module.css";

type SaveCircuitDialogProps = {
  onConfirm: (circuitName: string) => void;
  onDismiss: () => void;
};

export const SaveCircuitDialog: React.FC<SaveCircuitDialogProps> = (props) => {
  const { onDismiss, onConfirm } = props;
  const circuitNameInput = React.useRef<HTMLInputElement>(null);

  return (
    <div className={styles.modal}>
      <div className={styles.modalMain}>
        <div className="circuitNameInput">
          <input
            className={styles.circuitNameInput}
            type="text"
            ref={circuitNameInput}
            autoFocus={true}
          />
        </div>
        <div className={styles.actionButtons}>
          <div className={styles.actionButton}>
            <Button
              text="CANCEL"
              appearance="secondary"
              fullWidth
              size="small"
              onClick={onDismiss}
            />
          </div>
          <div className={styles.actionButton}>
            <Button
              text="SAVE"
              appearance="secondary"
              fullWidth
              size="small"
              onClick={() =>
                onConfirm(circuitNameInput.current.value.toUpperCase())
              }
            />
          </div>
        </div>
      </div>
    </div>
  );
};
