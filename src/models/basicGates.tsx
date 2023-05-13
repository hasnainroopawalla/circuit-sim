import Pin from "../components/Pin";
import { State } from "../enums/State";

export interface BasicGate {
  inputPins: number;
  outputPins: number;
  action: (a: Pin[]) => State[];
}
