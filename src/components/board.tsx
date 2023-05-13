import Sketch from "react-p5";
import p5Types from "p5";
import Circuit from "./Circuit";

var circuit: Circuit;

function Board() {
  const setup = (p5: p5Types, canvasParentRef: Element) => {
    p5.createCanvas(p5.windowWidth, p5.windowHeight);

    circuit = new Circuit(p5);
    const buttonAnd = p5.createButton("AND");
    buttonAnd.position(0, 0);
    buttonAnd.mousePressed(() => circuit.addChip("AND"));
    const buttonNOT = p5.createButton("NOT");
    buttonNOT.position(0, 20);
    buttonNOT.mousePressed(() => circuit.addChip("NOT"));
    const buttonInput = p5.createButton("Input");
    buttonInput.position(0, 40);
    buttonInput.mousePressed(() => circuit.addInputPin("Output_0"));
    const buttonOutput = p5.createButton("Output");
    buttonOutput.position(0, 60);
    buttonOutput.mousePressed(() => circuit.addOutputPin("Output_0"));
  };

  const draw = (p5: p5Types) => {
    p5.background(255);
    circuit.execute();
    circuit.render();
  };

  const mouseClicked = () => {
    circuit.mouseClicked();
  };

  const mouseDragged = () => {
    circuit.mouseDragged();
  };

  const mouseReleased = () => {
    circuit.mouseReleased();
  };

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

export default Board;
