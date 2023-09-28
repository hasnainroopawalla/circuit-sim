import React from "react";
import { Button } from "./button";
import { circuit } from "../simulator/sketch";
import { CORE_GATES } from "../simulator/core-gates";

const styles = {
  toolbarContainer: {
    color: "#fff",
    position: "absolute",
    bottom: "0",
    left: "0",
    right: "0",
    margin: "5px 10px",
    display: "flex",
    flexDirection: "row",
    gap: "10px",
  },
} as const;

export const Toolbar = () => {
  return (
    <div className="toolbar-container" style={styles.toolbarContainer}>
      <Button text="SAVE" color="#525151" onClick={() => alert("save")} />
      <Button
        text="AND"
        color={CORE_GATES["AND"].color}
        onClick={() => circuit.addCoreChip("AND")}
      />
      <Button
        text="OR"
        color={CORE_GATES["OR"].color}
        onClick={() => circuit.addCoreChip("OR")}
      />
      <Button
        text="NOT"
        color={CORE_GATES["NOT"].color}
        onClick={() => circuit.addCoreChip("NOT")}
      />
    </div>
  );
};
