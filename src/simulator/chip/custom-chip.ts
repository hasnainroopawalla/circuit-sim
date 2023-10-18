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
      input.id = `${id}.input.${i}`;
      input.name = `${id}.input.${i}`;
      input.isInput = true;
      input.pin.isInput = true;
      input.pin.id = 0;
      this.inputPins[i] = input.pin;
    }

    for (let i = 0; i < this.circuit.outputs.length; i++) {
      const output = this.circuit.outputs[i];
      output.id = `${id}.output.${i}`;
      output.name = `${id}.output.${i}`;
      output.isInput = false;
      output.pin.isInput = false;
      output.pin.id = 0;
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
