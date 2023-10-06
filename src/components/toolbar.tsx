import React, { useState } from "react";
import { CORE_GATES } from "../simulator";
import { EmitterEvent, emitter } from "../event-service";
import { useEventListener } from "./use-event-listener";
import { SaveCircuitDialog } from "./save-circuit-dialog";
import { Chip } from "./chip";
import { Button } from "./button";

const styles = {
  toolbarContainer: {
    color: "#fff",
    position: "absolute",
    bottom: "0",
    left: "0",
    right: "0",
    margin: "0.4rem 0.6rem",
    display: "flex",
    flexDirection: "row",
    gap: "0.5rem",
  },
} as const;

export const Toolbar = () => {
  const data = useEventListener(EmitterEvent.CustomChipBlueprintGenerated);

  const [showSaveCircuitDialog, setSaveShowCircuitDialog] = useState(false);

  return (
    <>
      <div className="toolbar-container" style={styles.toolbarContainer}>
        <Button
          text="SAVE"
          color="#525151"
          appearance="primary"
          size="large"
          onClick={() => setSaveShowCircuitDialog(true)}
        />
        <Chip
          text="AND"
          backgroundColor={CORE_GATES["AND"].color}
          onClick={() =>
            emitter.emit(EmitterEvent.SpawnCoreChip, { coreChip: "AND" })
          }
        />
        <Chip
          text="OR"
          backgroundColor={CORE_GATES["OR"].color}
          onClick={() =>
            emitter.emit(EmitterEvent.SpawnCoreChip, { coreChip: "OR" })
          }
        />
        <Chip
          text="NOT"
          backgroundColor={CORE_GATES["NOT"].color}
          onClick={() =>
            emitter.emit(EmitterEvent.SpawnCoreChip, { coreChip: "NOT" })
          }
        />
        {data && (
          <Chip
            text="NAND"
            backgroundColor="blue"
            onClick={() =>
              emitter.emit(EmitterEvent.SpawnCustomChip, {
                customChipBlueprint: data.customChipBlueprint,
                // customChipBlueprint: `{"name":"NAND","color":"blue","inputs":[{"id":"input-0","pin":"input-0_pin-0"},{"id":"input-1","pin":"input-1_pin-0"}],"outputs":[{"id":"output-0","pin":"output-0_pin-0"}],"chips":[{"id":"chip-0","type":"AND","inputPins":["chip-0_input-pin-0","chip-0_input-pin-1"],"outputPins":["chip-0_output-pin-0"]},{"id":"chip-1","type":"NOT","inputPins":["chip-1_input-pin-0"],"outputPins":["chip-1_output-pin-0"]}],"wires":[["input-0_pin-0","chip-0_input-pin-0"],["input-1_pin-0","chip-0_input-pin-1"],["chip-0_output-pin-0","chip-1_input-pin-0"],["chip-1_output-pin-0","output-0_pin-0"]]}`,
              })
            }
          />
        )}
      </div>
      {showSaveCircuitDialog && (
        <SaveCircuitDialog
          onConfirm={(circuitName: string) => {
            emitter.emit(EmitterEvent.SaveCircuit, { name: circuitName });
            setSaveShowCircuitDialog(false);
          }}
          onDismiss={() => {
            setSaveShowCircuitDialog(false);
          }}
        />
      )}
    </>
  );
};
