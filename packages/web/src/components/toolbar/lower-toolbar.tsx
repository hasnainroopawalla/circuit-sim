import * as React from "react";
import { Button } from "../factory";
import styles from "./toolbar.module.css";
import { useChips } from "../hooks";

type LowerToolbarProps = {
  saveButtonOnClick: () => void;
  importChipButtonOnClick: () => void;
};

export const LowerToolbar: React.FC<LowerToolbarProps> = (props) => {
  const { saveButtonOnClick, importChipButtonOnClick } = props;

  const chips = useChips();

  return (
    <div className={`${styles.toolbar} ${styles.lowerToolbar}`}>
      <Button
        text="SAVE"
        appearance="primary"
        size="large"
        onClick={saveButtonOnClick}
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
        onClick={importChipButtonOnClick}
      />
    </div>
  );
};
