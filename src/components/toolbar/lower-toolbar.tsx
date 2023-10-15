import React from "react";
import { Button } from "../factory";
import { EmitterEvent, emitter } from "../../event-service";
import type { IUseCustomChips } from "../hooks";

import styles from "./toolbar.module.css";

type LowerToolbarProps = {
  useCustomChips: IUseCustomChips;
  importChipButtonOnClick: () => void;
};

export const LowerToolbar: React.FC<LowerToolbarProps> = (props) => {
  const { useCustomChips, importChipButtonOnClick } = props;

  const customChips = useCustomChips();

  const blueprint = JSON.stringify({
    current: {
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

  return (
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
      <Button
        text="NAND-NOT"
        appearance="secondary"
        size="large"
        onClick={() =>
          emitter.emit(EmitterEvent.SpawnCustomChip, {
            name: "NAND-NOT",
            blueprint,
            color: "blue",
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
        onClick={importChipButtonOnClick}
      />
    </div>
  );
};
