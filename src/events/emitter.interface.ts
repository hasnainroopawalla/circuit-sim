export type EventData = {
  // A core chip spawn button is pressed
  SpawnCoreChip: {
    coreChip: "AND" | "NOT" | "OR";
  };

  // A circuit chip spawn button is pressed
  SpawnCircuitChip: {
    name: string;
    blueprint: string;
    color: string;
  };

  // New circuit chip is imported from a blueprint
  ImportChip: {
    chipName: string;
    blueprint: string;
  };

  // Save circuit button is pressed
  SaveCircuit: { name: string; color?: string };

  // Circuit chip blueprint string generated
  AddCircuitChipToToolbar: {
    name: string;
    blueprint: string;
  };

  // Notification message to be displayed to the user
  Notification: { text: string }; // TODO: Add error type, reason, etc.
};

export type EventKey = keyof EventData;
