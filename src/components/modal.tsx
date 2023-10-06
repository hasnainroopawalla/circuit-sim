import React from "react";
import { Button } from "./button";

const styles = {
  modal: {
    position: "fixed",
    top: "0",
    left: "0",
    width: "100%",
    height: "100%",
    background: "rgba(0, 0, 0, 0.5)",
  },
  modalMain: {
    position: "fixed",
    background: "#1e1e1e",
    top: "50%",
    left: "50%",
    transform: "translate(-50%,-50%)",
    padding: "1rem",
  },
  chipNameInput: {
    backgroundColor: "rgba(0, 0, 0, 0.3)",
    color: "#fff",
    border: "0",
    fontSize: "2rem",
    padding: "0.6rem",
  },
  actionButtons: {
    display: "flex",
    justifyContent: "space-between",
    marginTop: "1rem",
  },
} as const;

type ModalProps = {
  onConfirm: () => void;
  onDismiss: () => void;
};

export const Modal: React.FC<ModalProps> = (props) => {
  const { onDismiss, onConfirm } = props;

  return (
    <div style={styles.modal}>
      <section style={styles.modalMain}>
        <div className="custom-chip-name-input">
          <input style={styles.chipNameInput} type="text" id="customChipName" />
        </div>
        <div className="action-buttons" style={styles.actionButtons}>
          <Button
            text="CANCEL"
            color="#2F85BD"
            appearance="secondary"
            onClick={onDismiss}
          />
          <Button
            text="CONFIRM"
            color="#2F85BD"
            appearance="primary"
            onClick={onConfirm}
          />
        </div>
      </section>
    </div>
  );
};
