export enum EmitterEvent {
  SpawnCoreChip = "SpawnCoreChip",
  Notification = "Notification",
}

export type EmitterEventArgs = {
  [EmitterEvent.SpawnCoreChip]: { coreChip: "AND" | "NOT" | "OR" };
  [EmitterEvent.Notification]: { message: string };
};
