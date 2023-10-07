import React, { useRef, useState } from "react";
import { EmitterEvent, EmitterEventArgs, emitter } from "../event-service";
import { useEventListener } from "./use-event-listener";
import { SaveCircuitDialog } from "./save-circuit-dialog";
import { Chip } from "./chip";
import { Button } from "./button";
import styles from "./toolbar.module.css";
import { colorGenerator } from "../color-generator";

export const Toolbar = () => {
  const newCustomChipData = useEventListener(
    EmitterEvent.CustomChipBlueprintGenerated
  );

  const [showSaveCircuitDialog, setSaveShowCircuitDialog] = useState(false);

  const [customChips, setCustomChips] = useState<
    EmitterEventArgs[EmitterEvent.CustomChipBlueprintGenerated][]
  >([]);

  const chipColors = useRef<{ [chipName: string]: string }>({});

  React.useEffect(() => {
    if (!newCustomChipData) {
      return;
    }
    setCustomChips((prevCustomChips) => [
      ...prevCustomChips,
      newCustomChipData,
    ]);

    chipColors.current[newCustomChipData.name] = colorGenerator.generate();
  }, [newCustomChipData]);

  return (
    <>
      <div className={styles.toolbar}>
        <Button
          text="SAVE"
          appearance="primary"
          size="large"
          onClick={() => setSaveShowCircuitDialog(true)}
        />
        <Chip
          text="AND"
          onClick={() =>
            emitter.emit(EmitterEvent.SpawnCoreChip, {
              coreChip: "AND",
            })
          }
        />
        <Chip
          text="OR"
          onClick={() =>
            emitter.emit(EmitterEvent.SpawnCoreChip, {
              coreChip: "OR",
            })
          }
        />
        <Chip
          text="NOT"
          onClick={() =>
            emitter.emit(EmitterEvent.SpawnCoreChip, {
              coreChip: "NOT",
            })
          }
        />
        {customChips.map((customChip) => (
          <Chip
            key={customChip.name}
            text={customChip.name}
            onClick={() =>
              emitter.emit(EmitterEvent.SpawnCustomChip, {
                blueprint: customChip.blueprint,
                color: chipColors.current[customChip.name],
              })
            }
          />
        ))}
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
