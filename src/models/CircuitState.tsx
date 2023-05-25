import Chip from "../components/Chip";
import IOChip from "../components/IOChip";
import Pin from "../components/Pin";
import { Position } from "./RenderOptions";

interface WiringMode {
  enabled: boolean;
  startPin?: Pin;
  endPin?: Pin;
  waypoints: Position[];
}

interface DraggingMode {
  enabled: boolean;
  chip?: Chip | IOChip;
}

interface SpawnChipMode {
  enabled: boolean;
  chips: Chip[];
}

export interface CircuitState {
  wiringMode: WiringMode;
  draggingMode: DraggingMode;
  spawnChipMode: SpawnChipMode;
}
