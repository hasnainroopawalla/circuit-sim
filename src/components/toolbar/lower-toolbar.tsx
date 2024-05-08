import * as React from "react";
import { Button } from "../factory";
import { emitter } from "@circuit-sim/events";
import styles from "./toolbar.module.css";
import type { IUseCircuitChips } from "../hooks";

type LowerToolbarProps = {
  useCircuitChips: IUseCircuitChips;
  saveButtonOnClick: () => void;
  importChipButtonOnClick: () => void;
};

export const LowerToolbar: React.FC<LowerToolbarProps> = (props) => {
  const { useCircuitChips, saveButtonOnClick, importChipButtonOnClick } = props;

  const circuitChips = useCircuitChips();

  return (
    <div className={`${styles.toolbar} ${styles.lowerToolbar}`}>
      <Button
        text="SAVE"
        appearance="primary"
        size="large"
        onClick={saveButtonOnClick}
      />
      {/* <Button
        text="OPTIONS"
        appearance="secondary"
        size="large"
        onClick={() => {}}
      /> */}
      <Button
        text="AND"
        appearance="secondary"
        size="large"
        onClick={() =>
          emitter.emit("SpawnChip", {
            kind: "core",
            name: "AND",
          })
        }
      />
      <Button
        text="OR"
        appearance="secondary"
        size="large"
        onClick={() =>
          emitter.emit("SpawnChip", {
            kind: "core",
            name: "OR",
          })
        }
      />
      <Button
        text="NOT"
        appearance="secondary"
        size="large"
        onClick={() =>
          emitter.emit("SpawnChip", {
            kind: "core",
            name: "NOT",
          })
        }
      />
      {circuitChips.map((circuitChip) => (
        <Button
          key={circuitChip.name}
          text={circuitChip.name}
          appearance="secondary"
          size="large"
          onClick={circuitChip.onClick}
        />
      ))}
      <Button
        dataTestId="importChipButton"
        text="+"
        appearance="primary"
        size="large"
        onClick={importChipButtonOnClick}
      />
    </div>
  );
};
