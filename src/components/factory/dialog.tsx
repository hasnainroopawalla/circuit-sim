import React from "react";
import styles from "./dialog.module.css";

type DialogProps = {
  content: React.ReactNode;
};

export const Dialog: React.FC<DialogProps> = ({ content }) => {
  return (
    <div className={styles.modal}>
      <div className={styles.modalMain}>{content}</div>
    </div>
  );
};
