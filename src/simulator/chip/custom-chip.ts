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

    for (let i = 0; i < this.circuit.inputs.length; i++) {
      const input = this.circuit.inputs[i];
      input.id = `chip.${circuit.name}.input.${i}`;
      input.name = `chip.${circuit.name}.input.${i}`;
      input.isInput = true;
      input.pin.isInput = true;
      input.pin.id = `pin.${i}`;
      this.inputPins[i] = input.pin;
    }

    for (let i = 0; i < this.circuit.outputs.length; i++) {
      const output = this.circuit.outputs[i];
      output.id = `chip.${circuit.name}.output.${i}`;
      output.name = `chip.${circuit.name}.output.${i}`;
      output.isInput = false;
      output.pin.isInput = false;
      output.pin.id = `pin.${i}`;
      this.outputPins[i] = output.pin;
    }

    // this.inputPins = this.circuit.inputs.map((input) => {
    //   input.id = id;
    //   input.pin.id = ``
    //   input.pin.isInput = true;
    //   return input.pin;
    // });
    // this.outputPins = this.circuit.outputs.map((output) => {
    //   output.pin.isInput = false;
    //   return output.pin;
    // });
    // console.log("INPUT", this.inputPins);
    // console.log("OUTPUT", this.outputPins);
  }

  public execute(): void {
    this.circuit.execute();
  }
}
