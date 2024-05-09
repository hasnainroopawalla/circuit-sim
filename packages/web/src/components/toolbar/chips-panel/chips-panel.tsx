import * as React from "react";
import { Button } from "../../factory";
import styles from "./chips-panel.module.css";
import { useChips } from "../../hooks";
import { useDialog } from "../../dialog";

export const ChipsPanel: React.FC = () => {
  const chips = useChips();

  const { openDialog } = useDialog();

  const showSaveCircuitDialog = React.useCallback(
    () => openDialog("saveCircuit"),
    [openDialog]
  );

  const showImportChipDialog = React.useCallback(
    () => openDialog("importChip"),
    [openDialog]
  );

  return (
    <div className={styles.chipsPanelContainer}>
      <Button
        text="MENU"
        appearance="primary"
        size="large"
        onClick={showSaveCircuitDialog}
      />
      {chips.map((chip) => (
        <Button
          key={chip.name}
          text={chip.name}
          appearance="secondary"
          size="large"
          onClick={chip.onClick}
        />
      ))}
      <Button
        dataTestId="importChipButton"
        text="+"
        appearance="primary"
        size="large"
        onClick={showImportChipDialog}
      />
    </div>
  );
};
