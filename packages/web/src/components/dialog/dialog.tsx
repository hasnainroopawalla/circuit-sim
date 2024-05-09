import * as React from "react";
import styles from "./dialog.module.css";
import { RxCross2 as CloseIcon } from "react-icons/rx";
import { useDialog } from "./dialog-context";
import { getCurrentPanelProps } from "./dialog-panel-props";

export const Dialog: React.FC = () => {
  const { currentPanel, closeDialog } = useDialog();

  const { component, Icon, title } = getCurrentPanelProps(currentPanel);

  return currentPanel ? (
    <div className={styles.modalContainer}>
      <div className={styles.modalDialog}>
        <div className={styles.modalHeader}>
          <div>
            <Icon /> <span>{title.toUpperCase()}</span>
          </div>
          <CloseIcon className={styles.clickableIcon} onClick={closeDialog} />
        </div>
        <div className={styles.modalContent}>{component}</div>
      </div>
    </div>
  ) : null;
};
