import React from "react";
import { Button } from "./button";
import { CORE_GATES } from "../simulator";
import { EmitterEvent, emitter } from "../event-service";
import { useEventListener } from "./use-event-listener";

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
  const data = useEventListener(EmitterEvent.CustomChipBlueprintGenerated);

  return (
    <div className="toolbar-container" style={styles.toolbarContainer}>
      <Button
        text="SAVE"
        color="#525151"
        onClick={() => emitter.emit(EmitterEvent.SaveCircuit)}
      />
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
      {data && (
        <Button
          text="NAND"
          color="blue"
          onClick={() =>
            emitter.emit(EmitterEvent.SpawnCustomChip, {
              customChipBlueprint: data.customChipBlueprint,
              // customChipBlueprint: `{"name":"NAND","color":"blue","inputs":[{"id":"input-0","pin":"input-0_pin-0"},{"id":"input-1","pin":"input-1_pin-0"}],"outputs":[{"id":"output-0","pin":"output-0_pin-0"}],"chips":[{"id":"chip-0","type":"AND","inputPins":["chip-0_input-pin-0","chip-0_input-pin-1"],"outputPins":["chip-0_output-pin-0"]},{"id":"chip-1","type":"NOT","inputPins":["chip-1_input-pin-0"],"outputPins":["chip-1_output-pin-0"]}],"wires":[["input-0_pin-0","chip-0_input-pin-0"],["input-1_pin-0","chip-0_input-pin-1"],["chip-0_output-pin-0","chip-1_input-pin-0"],["chip-1_output-pin-0","output-0_pin-0"]]}`,
            })
          }
        />
      )}
    </div>
  );
};
