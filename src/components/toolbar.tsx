import React from "react";
import { Button } from "./button";

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
      <Button text="SAVE" onClick={() => {}} />
      <Button text="AND" onClick={() => {}} />
      <Button text="OR" onClick={() => {}} />
      <Button text="NOT" onClick={() => {}} />
    </div>
  );
};
