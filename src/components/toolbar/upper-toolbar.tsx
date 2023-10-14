import React from "react";
import { Button } from "../factory";

import styles from "./toolbar.module.css";

type UpperToolbarProps = {
  saveButtonOnClick: () => void;
  optionsButtonOnClick: () => void;
};

export const UpperToolbar: React.FC<UpperToolbarProps> = (props) => {
  const { saveButtonOnClick, optionsButtonOnClick } = props;

  return (
    <div className={`${styles.toolbar} ${styles.upperToolbar}`}>
      <Button
        text="SAVE"
        appearance="primary"
        size="large"
        onClick={saveButtonOnClick}
      />
      <Button
        text="OPTIONS"
        appearance="secondary"
        size="large"
        onClick={optionsButtonOnClick}
      />
    </div>
  );
};
