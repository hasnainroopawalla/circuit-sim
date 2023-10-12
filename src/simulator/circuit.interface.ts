import { Position, Size } from "./shared.interface";

import { IOChip, Chip, CoreGate } from "./chip";
import { Pin } from "./pin";
import type { WireMarker } from "./wire";

export type WiringMode = {
  markers: WireMarker[];
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

export type CustomChipBlueprint = {
  name: string;
  inputs: { id: string; pin: string }[];
  outputs: { id: string; pin: string }[];
  chips: {
    id: string;
    coreGate: CoreGate;
    inputPins: string[];
    outputPins: string[];
  }[];
  wires: string[][];
};
