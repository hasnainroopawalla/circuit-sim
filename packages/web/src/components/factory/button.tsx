import * as React from "react";
import styles from "./button.module.css";

type ButtonProps = {
  text: string;
  onClick: () => void;
  appearance?: "primary" | "secondary" | "dark";
  size: "small" | "large";
  fullWidth?: boolean;
  dataTestId?: string;
};

export const Button: React.FC<ButtonProps> = (props) => {
  const {
    text,
    onClick,
    appearance = "primary",
    size,
    fullWidth,
    dataTestId,
  } = props;

  const classes = `${styles.button} ${styles[appearance]} ${styles[size]} ${
    fullWidth ? styles.fullWidth : ""
  }`;

  return (
    <button onClick={onClick} className={classes} data-testid={dataTestId}>
      {text}
    </button>
  );
};
