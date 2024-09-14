import p5 from "p5";
import { Pin, PinState } from "../../pin";
import { Chip } from "../base-chip";
import { CORE_GATES, ICoreGate } from "./core-gates";

type ICoreChipArgs = {
  p: p5;
  coreGate: ICoreGate;
  id: string;
};

export class CoreChip extends Chip {
  action: (inputPins: Pin[]) => PinState[];

  constructor(args: ICoreChipArgs) {
    const { p, id, coreGate } = args;

    const numInputPins = CORE_GATES[coreGate].inputPins;
    const numOutputPins = CORE_GATES[coreGate].outputPins;

    super({
      p,
      id,
      numInputPins,
      numOutputPins,
      name: coreGate,
      color: CORE_GATES[coreGate].color,
    });

    for (let i = 0; i < numInputPins; i++) {
      this.inputPins.push(new Pin(p, i, PinState.Low, true, this));
    }
    for (let i = 0; i < numOutputPins; i++) {
      this.outputPins.push(new Pin(p, i, PinState.Low, false, this));
    }
    this.action = CORE_GATES[coreGate].action;
  }

  public execute(): void {
    setTimeout(() => {
      const outputStates = this.action(this.inputPins);
      for (let i = 0; i < this.outputPins.length; i++) {
        this.outputPins[i].state = outputStates[i];
        this.outputPins[i].propagate();
      }
    });
  }
}
