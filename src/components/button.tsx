import React from "react";

const styles = (color: string, appearance: ButtonProps["appearance"]) => {
  return {
    button: {
      backgroundColor: appearance === "primary" ? color : "#1e1e1e",
      color: appearance === "primary" ? "#fff" : color,
      border: `0.1rem solid ${appearance === "primary" ? "#121212" : color}`,
      padding: "0.3rem 0.5rem",
      textAlign: "center",
      fontSize: "1rem",
      cursor: "pointer",
      borderRadius: "0.5rem",
      MozUserSelect: "none" /* firefox */,
      WebkitUserSelect: "none" /* Safari */,
      msUserSelect: "none" /* IE*/,
      userSelect: "none",
    },
  } as const;
};

type ButtonProps = {
  text: string;
  onClick: () => void;
  color: string;
  appearance: "primary" | "secondary";
};

export const Button: React.FC<ButtonProps> = (props) => {
  const { text, onClick, color, appearance } = props;

  return (
    <button style={styles(color, appearance).button} onClick={onClick}>
      {text}
    </button>
  );
};
