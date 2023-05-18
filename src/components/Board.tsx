import p5Types from "p5";
import { basicGates } from "./BasicGates";
import Circuit from "./Circuit";
import Button from "../factory/button";

class Board {
  p5: p5Types;
  circuit: Circuit;
  buttons: Button[];

  constructor(p5: p5Types, circuit: Circuit) {
    this.p5 = p5;
    this.circuit = circuit;
    this.buttons = [
      new Button(this.p5, "Create", () => console.log("click"), {
        position: {
          x: 10,
          y: this.p5.windowHeight - 40,
        },
        size: { w: 50, h: 30 },
        color: "#77DD77",
      }),
    ];
    for (let i = 0; i < basicGates.length; i++) {
      const basicGate = basicGates[i];
      this.buttons.push(
        new Button(this.p5, "Create", () => console.log(basicGate.name), {
          position: {
            x: 70 + i * 70,
            y: this.p5.windowHeight - 40,
          },
          size: { w: 50, h: 30 },
          color: "#77DD77",
        })
      );
    }
  }

  private renderButtons() {
    this.buttons.forEach((button) => {
      button.render();
    });
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
    this.circuit.execute();
    this.circuit.render();
    this.renderButtons();
  }
}

export default Board;
