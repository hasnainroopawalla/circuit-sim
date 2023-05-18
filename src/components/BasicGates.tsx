import { State } from "../enums/State";
import Pin from "./Pin";

export const basicGates = [
  {
    name: "AND",
    inputPins: 2,
    outputPins: 1,
    action: (inputPins: Pin[]) => [inputPins[0].state && inputPins[1].state],
  },
  {
    name: "OR",
    inputPins: 2,
    outputPins: 1,
    action: (inputPins: Pin[]) => [inputPins[0].state || inputPins[1].state],
  },
  {
    name: "NOT",
    inputPins: 1,
    outputPins: 1,
    action: (inputPins: Pin[]) => [
      inputPins[0].state === State.On ? State.Off : State.On,
    ],
  },
];
