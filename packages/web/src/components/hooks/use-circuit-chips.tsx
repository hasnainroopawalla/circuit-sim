import * as React from "react";
import { pubsub } from "@circuit-sim/pubsub";
import { useEventListener } from "./use-event-listener";
import { colorGenerator } from "../color-generator";

export const useCircuitChips = () => {
  const [circuitChips, setCircuitChips] = React.useState<
    { name: string; onClick: () => void }[]
  >([]);

  const newCircuitChipData = useEventListener("AddCircuitChipToToolbar");

  React.useEffect(() => {
    if (!newCircuitChipData) {
      return;
    }
    const color = colorGenerator.generate();

    const newChipData = {
      name: newCircuitChipData.name,
      onClick: () =>
        pubsub.publish("SpawnChip", {
          kind: "circuit",
          name: newCircuitChipData.name,
          blueprint: newCircuitChipData.blueprint,
          color,
        }),
    };
    setCircuitChips((prevCircuitChips) => [...prevCircuitChips, newChipData]);
  }, [newCircuitChipData]);

  return circuitChips;
};

export type IUseCircuitChips = typeof useCircuitChips;
