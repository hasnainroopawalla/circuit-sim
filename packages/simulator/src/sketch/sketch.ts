import p5 from "p5";
import { CircuitBoard, circuitBoardConfig } from "../circuit-board";
import { sketchConfig } from "./sketch.config";

export let circuitBoard: CircuitBoard;

export const sketch = (p: p5) => {
  p.setup = () => {
    p.createCanvas(p.windowWidth, p.windowHeight);

    circuitBoard = new CircuitBoard(p, "main", {
      position: {
        x: circuitBoardConfig.widthScale,
        y: circuitBoardConfig.heightScale,
      },
      size: {
        w: p.windowWidth - circuitBoardConfig.widthScale * 2,
        h: p.windowHeight - 65,
      },
    });
  };

  p.draw = () => {
    p.background(sketchConfig.background);
    circuitBoard.execute();
    circuitBoard.render();
  };

  p.mouseClicked = () => circuitBoard.mouseClicked();
  p.mouseDragged = () => circuitBoard.mouseDragged();
  p.mouseReleased = () => circuitBoard.mouseReleased();
  p.doubleClicked = () => circuitBoard.mouseDoubleClicked();
  p.mouseMoved = () => circuitBoard.mouseMoved();
};
