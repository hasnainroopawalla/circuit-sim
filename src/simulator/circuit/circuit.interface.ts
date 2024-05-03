
import { IOChip } from "../chip";

export enum Mode {
  Idle = "Idle",
  Reposition = "Reposition",
  Wiring = "Wiring",
  SpawnChip = "SpawnChip",
  SpawnIOChipHover = "SpawnIOChipHover",
}

// TODO: move to modes
export enum Interaction {
  Click = "Click",
  DoubleClick = "DoubleClick",
  Drag = "Drag",
  Move = "Move",
}

export type CustomChipSchema = {
  inputs: { id: string }[];
  outputs: { id: string }[];
  chips: {
    id: string;
    name: string;
  }[];
  wires: string[][];
};

export type CustomChipBlueprint = {
  [chipName: string]: CustomChipSchema;
};
