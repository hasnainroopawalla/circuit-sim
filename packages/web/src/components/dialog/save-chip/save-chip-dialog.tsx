import * as React from "react";
import { Button } from "../../factory";
import styles from "./save-chip-dialog.module.css";
import dialogStyles from "../dialog.module.css";

type SaveChipDialogProps = {
  onConfirm: (circuitName: string) => void;
};

export const SaveChipDialog: React.FC<SaveChipDialogProps> = (props) => {
  const { onConfirm } = props;

  const chipNameInput = React.useRef<HTMLInputElement>(null);

  return (
    <>
      <div className={styles.chipNameInput}>
        <input
          data-testid="save-chip-name-input"
          className={styles.chipNameInput}
          type="text"
          ref={chipNameInput}
          autoFocus={true}
          placeholder={`CHIP NAME`}
        />
      </div>

      <div className={dialogStyles.dialogActionButton}>
        <Button
          dataTestId="save-chip-confirm-button"
          text="SAVE"
          appearance="dark"
          fullWidth
          size="small"
          onClick={() => {
            if (
              chipNameInput.current &&
              chipNameInput.current.value.length > 0
            ) {
              onConfirm(chipNameInput.current.value.toUpperCase());
            }
          }}
        />
      </div>
    </>
  );
};
