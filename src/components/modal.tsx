import React from "react";

// .modal {
//     position: fixed;
//     top: 0;
//     left: 0;
//     width:100%;
//     height: 100%;
//     background: rgba(0, 0, 0, 0.6);
//   }

//   .modal-main {
//     position:fixed;
//     background: white;
//     width: 80%;
//     height: auto;
//     top:50%;
//     left:50%;
//     transform: translate(-50%,-50%);
//   }

//   .display-block {
//     display: block;
//   }

//   .display-none {
//     display: none;
//   }

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
    background: "grey",
    width: "30%",
    height: "auto",
    top: "50%",
    left: "50%",
    transform: "translate(-50%,-50%)",
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
        <p>Modal!</p>
        <button type="button" onClick={onConfirm}>
          Confirm
        </button>
        <button type="button" onClick={onDismiss}>
          Close
        </button>
      </section>
    </div>
  );
};
