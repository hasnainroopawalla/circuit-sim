import Circuit from "./circuit";
import config from "../config";

export let circuit: Circuit;

export const sketch = (p: p5) => {
  p.setup = () => {
    p.createCanvas(p.windowWidth, p.windowHeight);

    circuit = new Circuit(p, "Initial", {
      position: {
        x: config.component.circuit.widthScale,
        y: config.component.circuit.widthScale,
      },
      size: {
        w: p.windowWidth - config.component.circuit.widthScale * 2,
        h: p.windowHeight - config.component.circuit.widthScale * 2,
      },
      color: "grey",
    });
  };

  p.draw = () => {
    p.background(config.document.color.background);
    circuit.execute();
    circuit.render();
  };

  p.mouseClicked = () => circuit.mouseClicked();
  p.mouseDragged = () => circuit.mouseDragged();
  p.mouseReleased = () => circuit.mouseReleased();
  // TODO: p.doubleClicked = () => circuit.doubleClicked();
};
