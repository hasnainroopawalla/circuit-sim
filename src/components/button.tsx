import React from "react";

// TODO: use rem instead of px
const styles = (color: string) => {
  return {
    button: {
      backgroundColor: color, // #525151
      border: "0.1rem solid #121212",
      color: "#fff",
      padding: "5px 10px",
      textAlign: "center",
      fontSize: "20px",
      cursor: "pointer",
      MozUserSelect: "none" /* firefox */,
      WebkitUserSelect: "none" /* Safari */,
      msUserSelect: "none" /* IE*/,
      userSelect: "none",
    },
  } as const;
};

type ButtonProps = {
  text: string;
  color: string;
  onClick: () => void;
};

export const Button: React.FC<ButtonProps> = (props) => {
  const { text, onClick, color } = props;

  return (
    <button style={styles(color).button} onClick={onClick}>
      {text}
    </button>
  );
};
