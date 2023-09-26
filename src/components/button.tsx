import React from "react";

const styles = {
  button: {
    backgroundColor: "#525151",
    border: "0.1rem solid #121212",
    color: "#fff",
    padding: "5px 10px",
    textAlign: "center",
    fontSize: "20px",
    cursor: "pointer",
  },
} as const;

type IButtonProps = {
  text: string;
  onClick: () => void;
};

export const Button: React.FC<IButtonProps> = (props) => {
  const { text, onClick } = props;

  return (
    <button style={styles.button} onClick={onClick}>
      {text}
    </button>
  );
};
