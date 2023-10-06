import React from "react";

const styles = (
  color: string,
  appearance: ButtonProps["appearance"],
  size: ButtonProps["size"]
) => {
  return {
    button: {
      backgroundColor: appearance === "primary" ? color : "#1e1e1e",
      color: appearance === "primary" ? "#fff" : color,
      border: `0.1rem solid ${appearance === "primary" ? "#121212" : color}`,
      padding: "0.3rem 0.5rem",
      textAlign: "center",
      fontSize: size === "small" ? "1rem" : "1.25rem",
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
  size: "small" | "large";
};

export const Button: React.FC<ButtonProps> = (props) => {
  const { text, onClick, color, appearance, size } = props;

  return (
    <button style={styles(color, appearance, size).button} onClick={onClick}>
      {text}
    </button>
  );
};
