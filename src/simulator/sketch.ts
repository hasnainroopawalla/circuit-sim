import Circuit from "./circuit";
import config from "../config";
import { Position, Size } from "./shared.interface";
import Board from "./board";

let board: Board;
export let circuit: Circuit;

export const sketch = (p: p5) => {
  p.setup = () => {
    p.createCanvas(p.windowWidth, p.windowHeight);

    const boardStartPosition: Position = {
      x: config.component.circuit.widthScale,
      y: config.component.circuit.widthScale,
    };
    const boardSize: Size = {
      w: p.windowWidth - config.component.circuit.widthScale * 2,
      h: p.windowHeight - config.component.circuit.widthScale * 2,
    };

    circuit = new Circuit(p, {
      position: boardStartPosition,
      size: boardSize,
    });
    board = new Board(p, circuit);
  };

  p.draw = () => {
    p.background(config.document.color.background);
    board.render();
  };

  p.mouseClicked = () => board.mouseClicked();
  p.mouseDragged = () => board.mouseDragged();
  p.mouseReleased = () => board.mouseReleased();
};
