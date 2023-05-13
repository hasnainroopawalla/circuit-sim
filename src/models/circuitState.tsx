import Chip from "../components/Chip";
import IOChip from "../components/IOChip";
import Pin from "../components/Pin";

interface WiringMode {
  enabled: boolean;
  startPin?: Pin;
  endPin?: Pin;
}

interface DraggingMode {
  enabled: boolean;
  chip?: Chip | IOChip;
}

export interface CircuitState {
  wiringMode: WiringMode;
  draggingMode: DraggingMode;
}
