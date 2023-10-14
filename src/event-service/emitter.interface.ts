export enum EmitterEvent {
  SpawnCoreChip = "SpawnCoreChip", // A core chip spawn button is pressed
  SpawnCustomChip = "SpawnCustomChip", // A custom chip spawn button is pressed
  SaveCircuit = "SaveCircuit", // Save circuit button is pressed
  AddCustomChipToToolbar = "AddCustomChipToToolbar", // Custom chip blueprint string generated
  Notification = "Notification", // Notification message to be displayed to the user
  ImportCustomChip = "ImportCustomChip", // New custom chip is imported from a blueprint
}

export type EmitterEventArgs = {
  [EmitterEvent.SpawnCoreChip]: {
    coreChip: "AND" | "NOT" | "OR";
  };
  [EmitterEvent.SpawnCustomChip]: {
    name: string;
    blueprint: string;
    color: string;
  };
  [EmitterEvent.ImportCustomChip]: {
    customChipName: string;
    blueprint: string;
  };
  [EmitterEvent.SaveCircuit]: { name: string; color?: string };
  [EmitterEvent.AddCustomChipToToolbar]: {
    name: string;
    blueprint: string;
  };
  [EmitterEvent.Notification]: { message: string }; // TODO: Add error type, reason, etc.
};
