import Chip from "./chip";
import IOChip from "./io-chip";
import Pin from "./pin";
import { IPosition } from "./render-options.interface";

export type WiringMode = {
  waypoints: IPosition[];
  startPin?: Pin;
  endPin?: Pin;
};

export type DraggingMode = {
  enabled: boolean;
  chip?: Chip | IOChip;
};

export type SpawnChipsMode = {
  chips: Chip[];
};
