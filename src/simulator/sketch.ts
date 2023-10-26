import { Circuit } from "./circuit";
import { config } from "../config";

export let circuit: Circuit;

export const sketch = (p: p5) => {
  p.setup = () => {
    p.createCanvas(p.windowWidth, p.windowHeight);

    circuit = new Circuit(p, "main", {
      position: {
        x: config.component.circuit.widthScale,
        y: 10,
      },
      size: {
        w: p.windowWidth - config.component.circuit.widthScale * 2,
        h: p.windowHeight - 65,
      },
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
  p.doubleClicked = () => circuit.mouseDoubleClicked();
  p.mouseMoved = () => circuit.mouseMoved();
};
