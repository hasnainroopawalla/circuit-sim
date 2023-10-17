import React from "react";
import { Button } from "../factory";
import styles from "./import-chip-dialog.module.css";

type ImportChipDialog = {
  onConfirm: (customChipName: string, blueprint: string) => void;
  onDismiss: () => void;
};

export const ImportChipDialog: React.FC<ImportChipDialog> = (props) => {
  const { onDismiss, onConfirm } = props;
  const chipNameInput = React.useRef<HTMLInputElement>(null);
  const blueprintInput = React.useRef<HTMLInputElement>(null);
  const blueprint = JSON.stringify({
    main: {
      inputs: [
        {
          id: "input-0",
          pin: "input-0_pin-0",
        },
        {
          id: "input-1",
          pin: "input-1_pin-0",
        },
      ],
      outputs: [
        {
          id: "output-0",
          pin: "output-0_pin-0",
        },
      ],
      chips: [
        {
          id: "chip-0",
          name: "NAND",
          inputPins: ["chip-0_input-pin-0", "chip-0_input-pin-1"],
          outputPins: ["chip-0_output-pin-0"],
        },
        {
          id: "chip-1",
          name: "NOT",
          inputPins: ["chip-1_input-pin-0"],
          outputPins: ["chip-1_output-pin-0"],
        },
      ],
      wires: [
        ["input-1_pin-0", "chip-0_input-pin-1"],
        ["input-0_pin-0", "chip-0_input-pin-0"],
        ["chip-0_output-pin-0", "chip-1_input-pin-0"],
        ["chip-1_output-pin-0", "output-0_pin-0"],
      ],
    },
    NAND: {
      inputs: [
        {
          id: "input-0",
          pin: "input-00_pin-0",
        },
        {
          id: "input-1",
          pin: "input-1_pin-0",
        },
      ],
      outputs: [
        {
          id: "output-0",
          pin: "output-0_pin-0",
        },
      ],
      chips: [
        {
          id: "chip-0",
          name: "AND",
          inputPins: ["chip-0_input-pin-0", "chip-0_input-pin-1"],
          outputPins: ["chip-0_output-pin-0"],
        },
        {
          id: "chip-1",
          name: "NOT",
          inputPins: ["chip-1_input-pin-0"],
          outputPins: ["chip-1_output-pin-0"],
        },
      ],
      wires: [
        ["input-1_pin-0", "chip-0_input-pin-1"],
        ["input-0_pin-0", "chip-0_input-pin-0"],
        ["chip-0_output-pin-0", "chip-1_input-pin-0"],
        ["chip-1_output-pin-0", "output-0_pin-0"],
      ],
    },
  });

  const blueprintNAND = JSON.stringify({
    main: {
      inputs: [
        {
          id: "chip.input.0",
        },
        {
          id: "chip.input.1",
        },
      ],
      outputs: [
        {
          id: "chip.output.0",
        },
      ],
      chips: [
        {
          id: "chip.AND.0",
          name: "AND",
        },
        {
          id: "chip.NOT.1",
          name: "NOT",
        },
      ],
      wires: [
        ["chip.input.0-pin.0", "chip.AND.0-inputPin.0"],
        ["chip.input.1-pin.0", "chip.AND.0-inputPin.1"],
        ["chip.AND.0-outputPin.0", "chip.NOT.1-inputPin.0"],
        ["chip.NOT.1-outputPin.0", "chip.output.0-pin.0"],
      ],
    },
  });

  return (
    <>
      <div className={styles.inputsContainer}>
        <input
          className={styles.chipNameInput}
          type="text"
          ref={chipNameInput}
          autoFocus={true}
          placeholder={`CHIP NAME`}
        />
        <input
          className={styles.blueprintInput}
          type="text"
          ref={blueprintInput}
          autoFocus={true}
          placeholder={`BLUEPRINT`}
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
                chipNameInput.current &&
                chipNameInput.current.value.length > 0 &&
                blueprintInput.current &&
                blueprintInput.current.value.length > 0
              ) {
                onConfirm(Date.now().toString(), blueprintNAND);
              }
            }}
          />
        </div>
      </div>
    </>
  );
};
