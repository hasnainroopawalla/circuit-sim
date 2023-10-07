import React from "react";
import styles from "./button.module.css";

type ButtonProps = {
  text: string;
  onClick: () => void;
  appearance?: "primary" | "secondary";
  size: "small" | "large";
  fullWidth?: boolean;
};

export const Button: React.FC<ButtonProps> = (props) => {
  const { text, onClick, appearance = "primary", size, fullWidth } = props;

  const classes = `${styles.button} ${
    appearance === "primary" ? styles.primary : styles.secondary
  } ${fullWidth ? styles.fullWidth : ""} ${
    size === "small" ? styles.small : styles.large
  }`;

  return (
    <button onClick={onClick} className={classes}>
      {text}
    </button>
  );
};
