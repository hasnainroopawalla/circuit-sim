import Chip from "./chip";
import IOChip from "./io-chip";
import Pin from "./pin";
import { IPosition } from "./render-options.interface";

type IWiringMode = {
  enabled: boolean;
  startPin?: Pin;
  endPin?: Pin;
  waypoints: IPosition[];
};

type IDraggingMode = {
  enabled: boolean;
  chip?: Chip | IOChip;
};

type ISpawnChipMode = {
  enabled: boolean;
  chips: Chip[];
};

export type ICircuitState = {
  wiringMode: IWiringMode;
  draggingMode: IDraggingMode;
  spawnChipMode: ISpawnChipMode;
};
