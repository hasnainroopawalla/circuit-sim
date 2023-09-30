import { Position, Size } from "./shared.interface";

import Chip from "./chip";
import IOChip from "./io-chip";
import Pin from "./pin";

export type WiringMode = {
  waypoints: Position[];
  startPin?: Pin;
  endPin?: Pin;
};

export type RepositionMode = {
  chip?: Chip | IOChip;
};

export type SpawnChipsMode = {
  chips: Chip[];
};

export type CircuitRenderOptions = {
  position: Position;
  size: Size;
};

export enum Mode {
  Idle = "Idle",
  Reposition = "Reposition",
  Wiring = "Wiring",
  SpawnChips = "SpawnChips",
}

export enum Interaction {
  Click = "Click",
  Drag = "Drag",
}
