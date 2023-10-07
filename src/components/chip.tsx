import React from "react";
import styles from "./chip.module.css";

const computedStyles = (color: string): React.CSSProperties => ({
  backgroundColor: color,
});

type ChipProps = {
  text: string;
  onClick: () => void;
  color?: string;
};

export const Chip: React.FC<ChipProps> = (props) => {
  const { text, onClick, color = "#525151" } = props;

  return (
    <button
      className={styles.chip}
      style={computedStyles(color)}
      onClick={onClick}
    >
      {text}
    </button>
  );
};
