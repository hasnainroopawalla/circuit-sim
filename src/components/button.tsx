import React from "react";
import styles from "./button.module.css";

// TODO: Move to utils and better type defs for this method
const computedStyles = (
  color: ButtonProps["color"],
  appearance: ButtonProps["appearance"],
  size: ButtonProps["size"],
  fullWidth: ButtonProps["fullWidth"]
): React.CSSProperties => ({
  backgroundColor: appearance === "primary" ? color : "#1e1e1e",
  color: appearance === "primary" ? "#fff" : color,
  border: `0.1rem solid ${appearance === "primary" ? "#121212" : color}`,
  fontSize: size === "small" ? "1rem" : "1.25rem",
  width: fullWidth ? "100%" : null,
});

type ButtonProps = {
  text: string;
  onClick: () => void;
  color: string;
  appearance?: "primary" | "secondary";
  size: "small" | "large";
  fullWidth?: boolean;
};

export const Button: React.FC<ButtonProps> = (props) => {
  const {
    text,
    onClick,
    color,
    appearance = "primary",
    size,
    fullWidth,
  } = props;

  return (
    <button
      onClick={onClick}
      className={styles.button}
      style={computedStyles(color, appearance, size, fullWidth)}
    >
      {text}
    </button>
  );
};
