import { Circuit, circuitConfig } from "../circuit";
import { sketchConfig } from "./sketch.config";

export let circuit: Circuit;

export const sketch = (p: p5) => {
  p.setup = () => {
    p.createCanvas(p.windowWidth, p.windowHeight);

    circuit = new Circuit(p, "main", {
      position: {
        x: circuitConfig.widthScale,
        y: circuitConfig.heightScale,
      },
      size: {
        w: p.windowWidth - circuitConfig.widthScale * 2,
        h: p.windowHeight - 65,
      },
    });
  };

  p.draw = () => {
    p.background(sketchConfig.background);
    circuit.execute();
    circuit.render();
  };

  p.mouseClicked = () => circuit.mouseClicked();
  p.mouseDragged = () => circuit.mouseDragged();
  p.mouseReleased = () => circuit.mouseReleased();
  p.doubleClicked = () => circuit.mouseDoubleClicked();
  p.mouseMoved = () => circuit.mouseMoved();
};
