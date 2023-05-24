import Sketch from "react-p5";
import p5Types from "p5";
import Circuit from "./Circuit";
import config from "../config";
import { Position, Size } from "../models/RenderOptions";
import Board from "./Board";

var board: Board;

function Simulator() {
  const setup = (p5: p5Types, canvasParentRef: Element) => {
    p5.createCanvas(p5.windowWidth, p5.windowHeight);

    const boardStartPosition: Position = {
      x: config.component.circuit.widthScale,
      y: config.component.circuit.widthScale,
    };
    const boardSize: Size = {
      w: p5.windowWidth - config.component.circuit.widthScale * 2,
      h: p5.windowHeight - config.component.circuit.widthScale * 2,
    };

    const circuit = new Circuit(p5, {
      position: boardStartPosition,
      size: boardSize,
    });
    board = new Board(p5, circuit);
  };

  const draw = (p5: p5Types) => {
    p5.background(config.document.color.background);
    board.render();
  };

  const mouseClicked = () => board.mouseClicked();

  const mouseDragged = () => board.mouseDragged();

  const mouseReleased = () => board.mouseReleased();

  return (
    <Sketch
      setup={setup}
      draw={draw}
      mouseClicked={mouseClicked}
      mouseDragged={mouseDragged}
      mouseReleased={mouseReleased}
    />
  );
}

export default Simulator;
