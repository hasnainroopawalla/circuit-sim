import { State } from "./shared.interface";

import Pin from "./pin";

export const CORE_GATES = {
  AND: {
    inputPins: 2,
    outputPins: 1,
    action: (inputPins: Pin[]) => [inputPins[0].state && inputPins[1].state],
    color: "#2F85BD",
  },
  OR: {
    inputPins: 2,
    outputPins: 1,
    action: (inputPins: Pin[]) => [inputPins[0].state || inputPins[1].state],
    color: "#A20F52",
  },
  NOT: {
    inputPins: 1,
    outputPins: 1,
    action: (inputPins: Pin[]) => [
      inputPins[0].state === State.On ? State.Off : State.On,
    ],
    color: "#7A7449",
  },
};
