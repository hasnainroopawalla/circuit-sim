import p5 from "p5";
import BlueprintHelper from "./blueprintHelper";

// /** Function that may be called on any keyboard event. */
// type KeyboardEventCallback = (event?: KeyboardEvent) => void | boolean;

// /** Function that may be called on any mouse event. */
// type MouseEventCallback = (event?: MouseEvent) => void | boolean;

// /** Function that may be called on any touch event. */
// type TouchEventCallback = (event?: TouchEvent) => void | boolean;

// /**
//  * Methods of `p5` that may be overwritten in `new p5()`.
//  */
// export type P5WritableMethods = {
//   preload: () => void;
//   setup: () => void;
//   draw: () => void;

//   windowResized: (event?: Event) => void;

//   keyPressed: KeyboardEventCallback;
//   keyReleased: KeyboardEventCallback;
//   keyTyped: KeyboardEventCallback;

//   mouseMoved: MouseEventCallback;
//   mouseDragged: MouseEventCallback;
//   mousePressed: MouseEventCallback;
//   mouseReleased: MouseEventCallback;
//   mouseClicked: MouseEventCallback;
//   doubleClicked: MouseEventCallback;
//   mouseWheel: MouseEventCallback;

//   touchStarted: TouchEventCallback;
//   touchMoved: TouchEventCallback;
//   touchEnded: TouchEventCallback;

//   deviceMoved: () => void;
//   deviceTurned: () => void;
//   deviceShaken: () => void;
// };

// export type SketchDef = {
//   [T in keyof P5WritableMethods]?: (
//     p: p5,
//     ...args: Parameters<P5WritableMethods[T]>
//   ) => ReturnType<P5WritableMethods[T]>;
// };

// export const createSketch = (definition: SketchDef) => {
//   const methodNames = Object.keys(definition) as (keyof SketchDef)[];

//   return (p: p5) => {
//     methodNames.forEach((methodName) => {
//       const method = definition[methodName];
//       if (method) p[methodName] = method.bind(undefined, p);
//     });
//   };
// };

const sketch1 = (p: p5) => {
  p.setup = () => {
    // p.createCanvas(p.windowWidth, p.windowHeight);
  };
  p.draw = () => {};
};

describe("Nodes & Edges tests", () => {
  let p: p5;

  beforeEach(() => {
    // const setup = (p: p5): void => {
    //   p.createCanvas(400, 400);
    // };
    // const draw = (p: p5): void => {
    //   p.createCanvas(800, 600);
    //   p.background(10);
    // };

    // const sketch = createSketch({
    //   setup,
    //   draw,
    // });
    expect(sketch1).not.toBeNull();
    p = new p5(sketch1);
  });

  test("hi", () => {
    const blueprint = {
      main: {
        inputs: [
          {
            id: "input-0",
            pin: "input-0_pin-0",
          },
          {
            id: "input-1",
            pin: "input-1_pin-0",
          },
        ],
        outputs: [
          {
            id: "output-0",
            pin: "output-0_pin-0",
          },
        ],
        chips: [
          {
            id: "chip-0",
            name: "NAND",
            inputPins: ["chip-0_input-pin-0", "chip-0_input-pin-1"],
            outputPins: ["chip-0_output-pin-0"],
          },
          {
            id: "chip-1",
            name: "NOT",
            inputPins: ["chip-1_input-pin-0"],
            outputPins: ["chip-1_output-pin-0"],
          },
        ],
        wires: [
          ["input-1_pin-0", "chip-0_input-pin-1"],
          ["input-0_pin-0", "chip-0_input-pin-0"],
          ["chip-0_output-pin-0", "chip-1_input-pin-0"],
          ["chip-1_output-pin-0", "output-0_pin-0"],
        ],
      },
      NAND: {
        inputs: [
          {
            id: "input-0",
            pin: "input-0_pin-0",
          },
          {
            id: "input-1",
            pin: "input-1_pin-0",
          },
        ],
        outputs: [
          {
            id: "output-0",
            pin: "output-0_pin-0",
          },
        ],
        chips: [
          {
            id: "chip-0",
            name: "AND",
            inputPins: ["chip-0_input-pin-0", "chip-0_input-pin-1"],
            outputPins: ["chip-0_output-pin-0"],
          },
          {
            id: "chip-1",
            name: "NOT",
            inputPins: ["chip-1_input-pin-0"],
            outputPins: ["chip-1_output-pin-0"],
          },
        ],
        wires: [
          ["input-1_pin-0", "chip-0_input-pin-1"],
          ["input-0_pin-0", "chip-0_input-pin-0"],
          ["chip-0_output-pin-0", "chip-1_input-pin-0"],
          ["chip-1_output-pin-0", "output-0_pin-0"],
        ],
      },
    };
    const customChip = BlueprintHelper.blueprintToCustomChip(
      p,
      `test-file`,
      "name",
      "color",
      blueprint["main"],
      blueprint
    );
    expect(customChip.id).toBe("test-file");
  });
});
