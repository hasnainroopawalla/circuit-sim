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
  circuitNameInput: {
    backgroundColor: "rgba(0, 0, 0, 0.3)",
    color: "#fff",
    border: "0",
    fontSize: "2rem",
    letterSpacing: "0.1rem",
    padding: "0.6rem",
  },
  actionButtons: {
    display: "flex",
    justifyContent: "space-between",
    marginTop: "1rem",
  },
} as const;

type ModalProps = {
  onConfirm: (circuitName: string) => void;
  onDismiss: () => void;
};

export const SaveCircuitDialog: React.FC<ModalProps> = (props) => {
  const { onDismiss, onConfirm } = props;
  const circuitNameInput = React.useRef<HTMLInputElement>(null);

  return (
    <div style={styles.modal}>
      <div style={styles.modalMain}>
        <div className="circuit-name-input">
          <input
            style={styles.circuitNameInput}
            type="text"
            ref={circuitNameInput}
            autoFocus={true}
          />
        </div>
        <div className="action-buttons" style={styles.actionButtons}>
          <Button
            text="CANCEL"
            color="#2F85BD"
            appearance="secondary"
            size="small"
            onClick={onDismiss}
          />
          <Button
            text="CONFIRM"
            color="#2F85BD"
            appearance="primary"
            size="small"
            onClick={() =>
              onConfirm(circuitNameInput.current.value.toUpperCase())
            }
          />
        </div>
      </div>
    </div>
  );
};
