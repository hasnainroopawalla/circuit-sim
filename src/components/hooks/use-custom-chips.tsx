import * as React from "react";
import { EmitterEvent, emitter } from "../../event-service";
import { useEventListener } from "./use-event-listener";
import { colorGenerator } from "../../color-generator";

export const useCustomChips = () => {
  const [customChips, setCustomChips] = React.useState<
    { name: string; onClick: () => void }[]
  >([]);

  const newCustomChipData = useEventListener(
    EmitterEvent.AddCustomChipToToolbar
  );

  React.useEffect(() => {
    if (!newCustomChipData) {
      return;
    }
    const color = colorGenerator.generate();

    const newChipData = {
      name: newCustomChipData.name,
      onClick: () =>
        emitter.emit(EmitterEvent.SpawnCustomChip, {
          name: newCustomChipData.name,
          blueprint: newCustomChipData.blueprint,
          color,
        }),
    };
    setCustomChips((prevCustomChips) => [...prevCustomChips, newChipData]);
  }, [newCustomChipData]);

  return customChips;
};

export type IUseCustomChips = typeof useCustomChips;
