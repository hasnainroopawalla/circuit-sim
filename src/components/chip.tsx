import React from "react";
import styles from "./chip.module.css";

const computedStyles = (backgroundColor: string): React.CSSProperties => ({
  backgroundColor,
});

type ChipProps = {
  text: string;
  onClick: () => void;
  backgroundColor: string;
};

export const Chip: React.FC<ChipProps> = (props) => {
  const { text, onClick, backgroundColor } = props;

  return (
    <button
      className={styles.chip}
      style={computedStyles(backgroundColor)}
      onClick={onClick}
    >
      {text}
    </button>
  );
};
