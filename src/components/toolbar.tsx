import React from "react";
import { EmitterEvent, emitter } from "../event-service";
import { useEventListener } from "./use-event-listener";
import { SaveCircuitDialog } from "./save-circuit-dialog";
import { Button } from "./button";
import styles from "./toolbar.module.css";
import { colorGenerator } from "../color-generator";

export const Toolbar = () => {
  const newCustomChipData = useEventListener(
    EmitterEvent.CustomChipBlueprintGenerated
  );

  const [showSaveCircuitDialog, setSaveShowCircuitDialog] =
    React.useState(false);

  const [customChips, setCustomChips] = React.useState<
    { name: string; onClick: () => void }[]
  >([]);

  React.useEffect(() => {
    if (!newCustomChipData) {
      return;
    }
    const color = colorGenerator.generate();

    const newChipData = {
      name: newCustomChipData.name,
      onClick: () =>
        emitter.emit(EmitterEvent.SpawnCustomChip, {
          blueprint: newCustomChipData.blueprint,
          color,
        }),
    };
    setCustomChips((prevCustomChips) => [...prevCustomChips, newChipData]);
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
        <Button
          text="AND"
          appearance="secondary"
          size="large"
          onClick={() =>
            emitter.emit(EmitterEvent.SpawnCoreChip, {
              coreChip: "AND",
            })
          }
        />
        <Button
          text="AND"
          appearance="secondary"
          size="large"
          onClick={() =>
            emitter.emit(EmitterEvent.SpawnCoreChip, {
              coreChip: "AND",
            })
          }
        />
        <Button
          text="OR"
          appearance="secondary"
          size="large"
          onClick={() =>
            emitter.emit(EmitterEvent.SpawnCoreChip, {
              coreChip: "OR",
            })
          }
        />
        <Button
          text="NOT"
          appearance="secondary"
          size="large"
          onClick={() =>
            emitter.emit(EmitterEvent.SpawnCoreChip, {
              coreChip: "NOT",
            })
          }
        />
        {customChips.map((customChip) => (
          <Button
            key={customChip.name}
            text={customChip.name}
            appearance="secondary"
            size="large"
            onClick={customChip.onClick}
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
