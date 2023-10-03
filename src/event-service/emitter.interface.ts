export enum EmitterEvent {
  SpawnCoreChip = "SpawnCoreChip", // Core chip spawn button is pressed
  SpawnCustomChip = "SpawnCustomChip", // Custom chip spawn button is pressed
  SaveCircuit = "SaveCircuit", // Save button is pressed
  CustomChipBlueprintGenerated = "CustomChipBlueprintGenerated", // Custom chip blueprint string generated
  Notification = "Notification", // Notification message to be displayed to the user
}

export type EmitterEventArgs = {
  [EmitterEvent.SpawnCoreChip]: { coreChip?: "AND" | "NOT" | "OR" };
  [EmitterEvent.SpawnCustomChip]: { customChipBlueprint?: string };
  [EmitterEvent.SaveCircuit]: void;
  [EmitterEvent.CustomChipBlueprintGenerated]: { customChipBlueprint?: string };
  [EmitterEvent.Notification]: { message?: string };
};
