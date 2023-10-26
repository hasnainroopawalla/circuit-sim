import * as React from "react";
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

  return (
    <div className={`${styles.toolbar} ${styles.lowerToolbar}`}>
      <Button
        text="SAVE"
        appearance="primary"
        size="large"
        onClick={() => {}}
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
        dataTestId="importChipButton"
        text="+"
        appearance="primary"
        size="large"
        onClick={importChipButtonOnClick}
      />
    </div>
  );
};
