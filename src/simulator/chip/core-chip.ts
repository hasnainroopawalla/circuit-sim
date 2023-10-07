import { State } from "../shared.interface";

import { Pin } from "../pin";
import { Chip } from "./chip";

export type CoreGate = "AND" | "OR" | "NOT";

const CORE_GATES = {
  AND: {
    inputPins: 2,
    outputPins: 1,
    action: (inputPins: Pin[]) => [inputPins[0].state && inputPins[1].state],
    color: "#fa8072",
  },
  OR: {
    inputPins: 2,
    outputPins: 1,
    action: (inputPins: Pin[]) => [inputPins[0].state || inputPins[1].state],
    color: "#a20f52",
  },
  NOT: {
    inputPins: 1,
    outputPins: 1,
    action: (inputPins: Pin[]) => [
      inputPins[0].state === State.On ? State.Off : State.On,
    ],
    color: "#daa520",
  },
};

export class CoreChip extends Chip {
  action: (inputPins: Pin[]) => State[];

  constructor(p: p5, coreGate: CoreGate, id: string) {
    const numInputPins = CORE_GATES[coreGate].inputPins;
    const numOutputPins = CORE_GATES[coreGate].outputPins;

    super(
      p,
      coreGate,
      id,
      numInputPins,
      numOutputPins,
      CORE_GATES[coreGate].color
    );

    for (let i = 0; i < numInputPins; i++) {
      this.inputPins.push(
        new Pin(p, `${id}_input-pin-${i}`, State.Off, true, this)
      );
    }
    for (let i = 0; i < numOutputPins; i++) {
      this.outputPins.push(
        new Pin(p, `${id}_output-pin-${i}`, State.Off, false, this)
      );
    }
    this.action = CORE_GATES[coreGate].action;
  }

  public execute(): void {
    const outputStates = this.action(this.inputPins);
    for (let i = 0; i < this.outputPins.length; i++) {
      this.outputPins[i].state = outputStates[i];
      this.outputPins[i].propagate();
    }
  }
}
