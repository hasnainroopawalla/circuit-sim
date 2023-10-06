import React from "react";

const styles = (backgroundColor: string) => {
  return {
    button: {
      backgroundColor,
      color: "#fff",
      border: "0.1rem solid #121212",
      padding: "0.3rem 0.5rem",
      textAlign: "center",
      fontSize: "1.25rem",
      cursor: "pointer",
      borderRadius: "0.5rem",
      MozUserSelect: "none" /* firefox */,
      WebkitUserSelect: "none" /* Safari */,
      msUserSelect: "none" /* IE*/,
      userSelect: "none",
    },
  } as const;
};

type ChipProps = {
  text: string;
  onClick: () => void;
  backgroundColor: string;
};

export const Chip: React.FC<ChipProps> = (props) => {
  const { text, onClick, backgroundColor } = props;

  return (
    <button style={styles(backgroundColor).button} onClick={onClick}>
      {text}
    </button>
  );
};
