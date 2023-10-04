import Circuit from "../circuit";
import { Chip } from "./chip";

export class CustomChip extends Chip {
  circuit: Circuit;

  constructor(p: p5, circuit: Circuit, id: string) {
    super(
      p,
      circuit.name,
      id,
      circuit.inputs.length,
      circuit.outputs.length,
      circuit.options.color
    );

    this.circuit = circuit;
    this.inputPins = this.circuit.inputs.map((input) => input.pin);
    this.outputPins = this.circuit.outputs.map((output) => output.pin);
    for (let i = 0; i < this.inputPins.length; i++) {
      this.inputPins[i].id = `${id}_input-pin-${i}`;
      this.inputPins[i].isInput = true;
    }
    for (let i = 0; i < this.outputPins.length; i++) {
      this.outputPins[i].id = `${id}_output-pin-${i}`;
      this.outputPins[i].isInput = false;
    }
  }

  public execute(): void {
    this.circuit.execute();
  }
}
