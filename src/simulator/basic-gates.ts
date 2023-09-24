import { State } from "../enums/state";
import Pin from "./pin";

type IBasicGate = {
  name: string;
  inputPins: number;
  outputPins: number;
  action: (a: Pin[]) => State[];
};

const basicGates: IBasicGate[] = [
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

export default basicGates;
