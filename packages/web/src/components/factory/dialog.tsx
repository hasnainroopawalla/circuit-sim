import * as React from "react";
import styles from "./dialog.module.css";
import type { IconType } from "react-icons";

type DialogProps = {
  title: string;
  icon: IconType;
  content: React.ReactNode;
};

export const Dialog: React.FC<DialogProps> = (props) => {
  const { title, icon: Icon, content } = props;
  return (
    <div className={styles.modalContainer}>
      <div className={styles.modalDialog}>
        <div className={styles.modalHeader}>
          {<Icon />} <span>{title.toUpperCase()}</span>
        </div>
        <div className={styles.modalContent}>{content}</div>
      </div>
    </div>
  );
};
