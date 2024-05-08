import type { Chip, IOChip } from "../chips";
import type { Pin } from "../pin";
import type { Wire } from "../wire";

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

// TODO: move to service
export type CustomChipBlueprint = {
  [chipName: string]: CustomChipSchema;
};

export type CircuitEntities = {
  inputs: IOChip[];
  outputs: IOChip[];
  wires: Wire[];
  chips: Chip[];
};

export type CircuitModeProps =
  | { mode: Mode.Idle; deps?: object }
  | { mode: Mode.SpawnChip; deps: { chip: Chip } }
  | { mode: Mode.Reposition; deps: { chip: Chip | IOChip } }
  | { mode: Mode.Wiring; deps: { startPin: Pin } }
  | { mode: Mode.SpawnIOChipHover; deps: { kind: "input" | "output" } };
