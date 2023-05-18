import Sketch from "react-p5";
import p5Types from "p5";
import Circuit from "./Circuit";
import config from "../config";
import { Position, Size } from "../models/RenderOptions";
import Board from "./Board";

var board: Board;

// TODO: Rename Board to Sketch or Simulator
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

    // const buttonAnd = p5.createButton("AND");
    // buttonAnd.position(0, 0);
    // buttonAnd.mousePressed(() => circuit.addChip("AND"));
    // const buttonNOT = p5.createButton("NOT");
    // buttonNOT.position(0, 20);
    // buttonNOT.mousePressed(() => circuit.addChip("NOT"));
    // const buttonInput = p5.createButton("Input");
    // buttonInput.position(0, 40);
    // buttonInput.mousePressed(() => circuit.addInputPin("Output_0"));
    // const buttonOutput = p5.createButton("Output");
    // buttonOutput.position(0, 60);
    // buttonOutput.mousePressed(() => circuit.addOutputPin("Output_0"));
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
