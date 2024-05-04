import { Circuit } from "../circuit";
import { Chip } from "./base-chip";

export class CustomChip extends Chip {
  circuit: Circuit;

  constructor(p: p5, circuit: Circuit, id: string, color: string) {
    super(
      p,
      circuit.name,
      id,
      circuit.entities.inputs.length,
      circuit.entities.outputs.length,
      color
    );

    this.circuit = circuit;
    
    for (let i = 0; i < this.circuit.entities.inputs.length; i++) {
      const input = this.circuit.entities.inputs[i];
      const newId = `${id}.input.${i}`;
      input.id = newId;
      input.isInput = true;
      input.pin.isInput = true;
      input.pin.id = 0;
      input.pin.name = `In ${i}`;
      this.inputPins[i] = input.pin;
    }

    for (let i = 0; i < this.circuit.entities.outputs.length; i++) {
      const output = this.circuit.entities.outputs[i];
      const newId = `${id}.output.${i}`;
      output.id = newId;
      output.isInput = false;
      output.pin.isInput = false;
      output.pin.id = 0;
      output.pin.name = `Out ${i}`;
      this.outputPins[i] = output.pin;
    }
  }

  public execute(): void {
    this.circuit.execute();
  }
}
