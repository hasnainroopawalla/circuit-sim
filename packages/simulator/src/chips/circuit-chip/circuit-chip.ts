import p5 from "p5";
import { Chip } from "../base-chip";
import type { ICircuitBoard } from "../../circuit-board";

export class CircuitChip extends Chip {
  circuitBoard: ICircuitBoard;

  constructor(p: p5, circuitBoard: ICircuitBoard, id: string, color: string) {
    super(
      p,
      circuitBoard.name,
      id,
      circuitBoard.entities.inputs.length,
      circuitBoard.entities.outputs.length,
      color
    );

    this.circuitBoard = circuitBoard;

    for (let i = 0; i < this.circuitBoard.entities.inputs.length; i++) {
      const input = this.circuitBoard.entities.inputs[i];
      const newId = `${id}.input.${i}`;
      input.id = newId;
      input.isInput = true;
      input.pin.isInput = true;
      input.pin.id = 0;
      input.pin.name = `In ${i}`;
      this.inputPins[i] = input.pin;
    }

    for (let i = 0; i < this.circuitBoard.entities.outputs.length; i++) {
      const output = this.circuitBoard.entities.outputs[i];
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
    this.circuitBoard.execute();
  }
}
