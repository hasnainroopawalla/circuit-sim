import { Position, Size } from "./shared.interface";

import { IOChip, Chip } from "./chip";
import Pin from "./pin";
import { CoreGate } from "./core-gates";

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
  color: string;
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
  color: string;
  inputs: [{ id: string; pin: string }];
  outputs: [{ id: string; pin: string }];
  chips: [
    { id: string; type: CoreGate; inputPins: string[]; outputPins: string[] }
  ];
  wires: string[][];
};
