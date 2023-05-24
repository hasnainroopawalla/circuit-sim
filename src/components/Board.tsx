import p5Types from "p5";
import { basicGates } from "./BasicGates";
import Circuit from "./Circuit";
import Button from "../factory/button";
import config from "../config";

class Board {
  p5: p5Types;
  circuit: Circuit;
  buttons: Button[];

  constructor(p5: p5Types, circuit: Circuit) {
    this.p5 = p5;
    this.circuit = circuit;
    this.buttons = [
      new Button(this.p5, "SAVE", () => this.createChipFromCircuit()),
    ];
    for (let i = 0; i < basicGates.length; i++) {
      const basicGate = basicGates[i];
      this.buttons.push(
        new Button(this.p5, basicGate.name, () =>
          this.circuit.addChip(
            basicGate.name,
            basicGate.inputPins,
            basicGate.outputPins,
            false,
            basicGate.action
          )
        )
      );
    }
  }

  private renderButtons() {
    let currButtonX = 10;
    for (let i = 0; i < this.buttons.length; i++) {
      this.buttons[i].options.position.x = currButtonX;
      currButtonX +=
        this.buttons[i].options.size.w +
        config.component.board.spacingBetweenButtons;
      this.buttons[i].options.position.y = this.p5.windowHeight - 40;
      this.buttons[i].render();
    }
  }

  public mouseClicked() {
    this.buttons.forEach((button) => {
      button.mouseClicked();
    });
    this.circuit.mouseClicked();
  }

  public mouseDragged() {
    this.circuit.mouseDragged();
  }

  public mouseReleased() {
    this.circuit.mouseReleased();
  }

  public render() {
    this.p5.stroke(config.document.strokeColor);
    this.p5.strokeWeight(config.document.strokeWeight);
    this.circuit.execute();
    this.circuit.render();
    this.renderButtons();
  }

  public createChipFromCircuit() {
    // TODO: Better new circuit handling
    let newCircuit = new Circuit(this.p5, this.circuit.options);
    newCircuit.inputs = this.circuit.inputs;
    newCircuit.outputs = this.circuit.outputs;
    newCircuit.wires = this.circuit.wires;
    newCircuit.chips = this.circuit.chips;

    this.buttons.push(
      new Button(this.p5, "NAND", () =>
        this.circuit.addChip("NAND", 1, 1, true, () => [], newCircuit)
      )
    );

    this.circuit.inputs = [];
    this.circuit.outputs = [];
    this.circuit.wires = [];
    this.circuit.chips = [];
  }
}

export default Board;
