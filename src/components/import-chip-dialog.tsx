import React from "react";
import { Button } from "./factory";
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
          placeholder="e.g. input" // TODO: fix
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
/*
{"name":"NAND","inputs":[{"id":"input-0","pin":"input-0_pin-0"},{"id":"input-1","pin":"input-1_pin-0"}],"outputs":[{"id":"output-0","pin":"output-0_pin-0"}],"chips":[{"id":"chip-0","coreGate":"AND","inputPins":["chip-0_input-pin-0","chip-0_input-pin-1"],"outputPins":["chip-0_output-pin-0"]},{"id":"chip-1","coreGate":"NOT","inputPins":["chip-1_input-pin-0"],"outputPins":["chip-1_output-pin-0"]}],"wires":[["input-1_pin-0","chip-0_input-pin-1"],["input-0_pin-0","chip-0_input-pin-0"],["chip-0_output-pin-0","chip-1_input-pin-0"],["chip-1_output-pin-0","output-0_pin-0"]]}
*/
