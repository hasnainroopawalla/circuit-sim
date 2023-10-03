import { State } from "../shared.interface";

import Pin from "../pin";
import { Chip } from "./chip";
import { CORE_GATES, CoreGate } from "../core-gates";

export class CoreChip extends Chip {
  constructor(p: p5, name: CoreGate, id: string) {
    super(
      p,
      name,
      "core-chip-id",
      CORE_GATES[name].inputPins,
      CORE_GATES[name].outputPins,
      CORE_GATES[name].action,
      CORE_GATES[name].color
    );
    // this.name = name;
    // this.id = id;
    // this.action = action;

    for (let i = 0; i < CORE_GATES[name].inputPins; i++) {
      this.inputPins.push(
        new Pin(p, `${id}_input-pin-${i}`, State.Off, true, this)
      );
    }
    for (let i = 0; i < CORE_GATES[name].outputPins; i++) {
      this.outputPins.push(
        new Pin(p, `${id}_output-pin-${i}`, State.Off, false, this)
      );
    }
  }

  public execute(): void {
    const outputStates = this.action(this.inputPins);
    for (let i = 0; i < this.outputPins.length; i++) {
      this.outputPins[i].state = outputStates[i];
      this.outputPins[i].propagate();
    }
  }
}
