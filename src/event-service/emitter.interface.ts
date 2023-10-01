export enum EmitterEvent {
  SpawnCoreChip = "SpawnCoreChip",
  SpawnCustomChip = "SpawnCustomChip",
  SaveCircuit = "SaveCircuit",
  Notification = "Notification",
}

export type EmitterEventArgs = {
  [EmitterEvent.SpawnCoreChip]: { coreChip: "AND" | "NOT" | "OR" };
  [EmitterEvent.SpawnCustomChip]: { customChipString: string };
  [EmitterEvent.SaveCircuit]: void;
  [EmitterEvent.Notification]: { message: string };
};
