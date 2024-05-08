import { State } from "../../common";
import type { Pin } from "../../pin";

export const CORE_GATES = {
  AND: {
    inputPins: 2,
    outputPins: 1,
    action: (inputPins: Pin[]) => [inputPins[0].state && inputPins[1].state],
    color: "#ff7f50",
  },
  OR: {
    inputPins: 2,
    outputPins: 1,
    action: (inputPins: Pin[]) => [inputPins[0].state || inputPins[1].state],
    color: "#008000",
  },
  NOT: {
    inputPins: 1,
    outputPins: 1,
    action: (inputPins: Pin[]) => [
      inputPins[0].state === State.On ? State.Off : State.On,
    ],
    color: "#a20f52",
  },
};
