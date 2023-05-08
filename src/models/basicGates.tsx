import Pin from "../components/pin";
import { State } from "../enums/state";

export interface BasicGate {
  inputPins: number;
  outputPins: number;
  action: (a: Pin[]) => State[];
}
