import { Position, Size } from "../shared.interface";

import { IOChip, Chip } from "../chip";
import { Pin } from "../pin";
import type { WireMarker } from "../wire";

export type WiringMode = {
  markers: WireMarker[];
  startPin?: Pin;
  endPin?: Pin;
};

export type RepositionMode = {
  chip?: Chip | IOChip;
};

export type SpawnChipMode = {
  chips: Chip[];
};

export type SpawnIOChipHoverMode = {
  chip?: IOChip;
  type?: "input" | "output";
};

export type CircuitRenderOptions = {
  position: Position;
  size: Size;
};

export enum Mode {
  Idle = "Idle",
  Reposition = "Reposition",
  Wiring = "Wiring",
  SpawnChip = "SpawnChip",
  SpawnIOChipHover = "SpawnIOChipHover",
}

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
