import { Circuit } from "../circuit";
import { Chip } from "./chip";

export class CustomChip extends Chip {
  circuit: Circuit;

  constructor(p: p5, circuit: Circuit, id: string, color: string) {
    super(
      p,
      circuit.name,
      id,
      circuit.inputs.length,
      circuit.outputs.length,
      color
    );

    this.circuit = circuit;
    this.inputPins = this.circuit.inputs.map((input) => {
      input.pin.isInput = true;
      return input.pin;
    });
    this.outputPins = this.circuit.outputs.map((output) => {
      output.pin.isInput = false;
      return output.pin;
    });
  }

  public execute(): void {
    this.circuit.execute();
  }
}
