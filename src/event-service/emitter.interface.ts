export enum EmitterEvent {
  SpawnCoreChip = "SpawnCoreChip", // A core chip spawn button is pressed
  SpawnCustomChip = "SpawnCustomChip", // A custom chip spawn button is pressed
  SaveCircuit = "SaveCircuit", // Save circuit button is pressed
  CustomChipBlueprintGenerated = "CustomChipBlueprintGenerated", // Custom chip blueprint string generated
  Notification = "Notification", // Notification message to be displayed to the user
  ImportCustomChip = "ImportCustomChip", // New custom chip is imported from a blueprint
}

export type EmitterEventArgs = {
  [EmitterEvent.SpawnCoreChip]: {
    coreChip: "AND" | "NOT" | "OR";
  };
  [EmitterEvent.SpawnCustomChip]: { blueprint: string; color: string };
  [EmitterEvent.ImportCustomChip]: { blueprint: string };
  [EmitterEvent.SaveCircuit]: { name: string; color?: string };
  [EmitterEvent.CustomChipBlueprintGenerated]: {
    name: string;
    blueprint: string;
  };
  [EmitterEvent.Notification]: { message: string }; // TODO: Add error type, reason, etc.
};
