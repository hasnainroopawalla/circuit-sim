import Sketch from "react-p5";
import p5Types from "p5";
import Circuit from "./circuit";

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
    //   circuit.addInputPin("Input_0");
    //   circuit.addInputPin("input_1");

    //   circuit.addChip(basicLogicGates.AND);
    //   circuit.addChip(basicLogicGates.NOT);

    //   circuit.addOutputPin("Output_0");

    //   // Input.0 -> AND_Input.0
    //   circuit.addWire(
    //     circuit.getInputPin(0).pin,
    //     circuit.getChip(0).getInputPin(0)
    //   );
    //   // Input.1 -> AND_Input.1
    //   circuit.addWire(
    //     circuit.getInputPin(1).pin,
    //     circuit.getChip(0).getInputPin(1)
    //   );
    //   // AND_Output.0 -> NOT_Input.0
    //   circuit.addWire(
    //     circuit.getChip(0).getOutputPin(0),
    //     circuit.getChip(1).getInputPin(0)
    //   );
    //   // NOT_Output.0 -> Output.0
    //   circuit.addWire(
    //     circuit.getChip(1).getOutputPin(0),
    //     circuit.getOutputPin(0).pin
    //   );
  };

  const draw = (p5: p5Types) => {
    p5.background(255);
    circuit.execute();
    circuit.render();
    if (circuit.getChip(0)) {
      circuit.getChip(0).options.position.x =
        circuit.getChip(0).options.position.x + 1;
    }
    // console.log(circuit.chips);
    // console.log(
    //   circuit.inputPins.map((inOut) => inOut.pin.state),
    //   circuit.outputPins.map((inOut) => inOut.pin.state)[0]
    // );
  };

  return <Sketch setup={setup} draw={draw} />;
}

export default Board;
