import p5 from "p5";
import BlueprintHelper from "./blueprintHelper";
import CircuitHelper from "./circuitHelper";

const sketch = (p: p5) => {
  p.setup = () => {};
  p.draw = () => {};
};

describe("Nodes & Edges tests", () => {
  let p: p5;

  beforeEach(() => {
    expect(sketch).not.toBeNull();
    p = new p5(sketch);
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
      `CustomChipId`,
      "NAND-NOT",
      "color",
      blueprint["main"],
      blueprint
    );
    console.log("FINAL", customChip);
    CircuitHelper.renderSummary(customChip.circuit);
    expect(customChip.id).toBe("test-file");
  });
});
