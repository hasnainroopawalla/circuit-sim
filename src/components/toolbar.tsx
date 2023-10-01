import React from "react";
import { Button } from "./button";
import { CORE_GATES } from "../simulator";
import { EmitterEvent, emitter } from "../event-service";

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
      <Button text="SAVE" color="#525151" onClick={() => {}} />
      <Button
        text="AND"
        color={CORE_GATES["AND"].color}
        onClick={() =>
          emitter.emit(EmitterEvent.SpawnCoreChip, { coreChip: "AND" })
        }
      />
      <Button
        text="OR"
        color={CORE_GATES["OR"].color}
        onClick={() =>
          emitter.emit(EmitterEvent.SpawnCoreChip, { coreChip: "OR" })
        }
      />
      <Button
        text="NOT"
        color={CORE_GATES["NOT"].color}
        onClick={() =>
          emitter.emit(EmitterEvent.SpawnCoreChip, { coreChip: "NOT" })
        }
      />
    </div>
  );
};
