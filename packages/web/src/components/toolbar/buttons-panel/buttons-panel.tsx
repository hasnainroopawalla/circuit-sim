import * as React from "react";
import { Button } from "../../factory";
import styles from "./buttons-panel.module.css";
import { useChips } from "../../hooks";
import { Menu } from "../menu";

export const ButtonsPanel: React.FC = () => {
  const chips = useChips();

  return (
    <div className={styles.buttonsPanelContainer}>
      <Menu />
      {chips.map((chip) => (
        <Button
          key={chip.name}
          text={chip.name}
          appearance="secondary"
          size="large"
          onClick={chip.onClick}
        />
      ))}
    </div>
  );
};
