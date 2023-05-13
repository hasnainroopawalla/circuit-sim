import Pin from "../components/pin";

interface WiringMode {
  enabled: boolean;
  startPin?: Pin;
  endPin?: Pin;
}

export interface ICircuitState {
  wiringMode: WiringMode;
}
