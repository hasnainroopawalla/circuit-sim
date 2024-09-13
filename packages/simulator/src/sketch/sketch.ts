import p5 from "p5";
import {
  circuitBoardConfig,
  createCircuitBoard,
  type ICircuitBoard,
} from "../circuit-board";
import { sketchConfig } from "./sketch.config";

let circuitBoard: ICircuitBoard;
let sketchInteractionEnabled = true;

export const setSketchInteraction = (interactionEnabled: boolean) =>
  (sketchInteractionEnabled = interactionEnabled);

const sketch = (p: p5) => {
  p.setup = () => {
    p.createCanvas(p.windowWidth, p.windowHeight);

    circuitBoard = createCircuitBoard({
      p,
      name: "main",
      options: {
        position: {
          x: circuitBoardConfig.widthScale,
          y: circuitBoardConfig.heightScale,
        },
        size: {
          w: p.windowWidth - circuitBoardConfig.widthScale * 2,
          h: p.windowHeight - 65,
        },
      },
    });

    // circuitBoard = new CircuitBoard(p, "main", {
    //   position: {
    //     x: circuitBoardConfig.widthScale,
    //     y: circuitBoardConfig.heightScale,
    //   },
    //   size: {
    //     w: p.windowWidth - circuitBoardConfig.widthScale * 2,
    //     h: p.windowHeight - 65,
    //   },
    // });
  };

  p.draw = () => {
    p.background(sketchConfig.background);
    circuitBoard.execute();
    circuitBoard.render();
  };

  p.mouseClicked = () =>
    sketchInteractionEnabled && circuitBoard.mouseClicked();
  // p.mouseDragged = () =>
  //   sketchInteractionEnabled && circuitBoard.mouseDragged();
  // p.mouseReleased = () =>
  //   sketchInteractionEnabled && circuitBoard.mouseReleased();
  // p.doubleClicked = () =>
  //   sketchInteractionEnabled && circuitBoard.mouseDoubleClicked();
  p.mouseMoved = () => sketchInteractionEnabled && circuitBoard.mouseMoved();
};

export const createP5Instance = (container: HTMLDivElement) =>
  new p5(sketch, container);
