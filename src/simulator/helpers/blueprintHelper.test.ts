import p5 from "p5";
import BlueprintHelper from "./blueprintHelper";
import { Circuit } from "../circuit";
import { idGenerator } from "./idGenerator";

const sketch = (p: p5) => {
  p.setup = () => {};
  p.draw = () => {};
};

describe("BlueprintHelper", () => {
  let p: p5;

  beforeEach(() => {
    p = new p5(sketch);
  });

  describe("circuitToBlueprint", () => {
    const BLUEPRINT_AND = {
      main: {
        inputs: [
          {
            id: "chip.input.0",
          },
          {
            id: "chip.input.1",
          },
        ],
        outputs: [
          {
            id: "chip.output.2",
          },
        ],
        chips: [
          {
            id: "chip.AND.3",
            name: "AND",
          },
        ],
        wires: [
          ["chip.input.0/output.0", "chip.AND.3/input.0"],
          ["chip.input.1/output.0", "chip.AND.3/input.1"],
          ["chip.AND.3/output.0", "chip.output.2/input.0"],
        ],
      },
    };

    const BLUEPRINT_NAND = {
      main: {
        inputs: [
          {
            id: "chip.input.0",
          },
          {
            id: "chip.input.1",
          },
        ],
        outputs: [
          {
            id: "chip.output.2",
          },
        ],
        chips: [
          {
            id: "chip.AND.3",
            name: "AND",
          },
          {
            id: "chip.NOT.4",
            name: "NOT",
          },
        ],
        wires: [
          ["chip.input.0/output.0", "chip.AND.3/input.0"],
          ["chip.input.1/output.0", "chip.AND.3/input.1"],
          ["chip.AND.3/output.0", "chip.NOT.4/input.0"],
          ["chip.NOT.4/output.0", "chip.output.2/input.0"],
        ],
      },
    };

    let circuit: Circuit;

    beforeEach(() => {
      idGenerator.reset();
      circuit = new Circuit(
        p,
        "main",
        {
          position: {
            x: 0,
            y: 0,
          },
          size: {
            w: 0,
            h: 0,
          },
        },
        true
      );
    });

    test("returns the blueprint for a custom AND chip", () => {
      const input0 = circuit.spawnInputIOChip();
      const input1 = circuit.spawnInputIOChip();
      const output0 = circuit.spawnOutputIOChip();
      const andChip = circuit.createCoreChip("AND");

      circuit.spawnWire(input0.pin, andChip.inputPins[0]);
      circuit.spawnWire(input1.pin, andChip.inputPins[1]);
      circuit.spawnWire(andChip.outputPins[0], output0.pin);

      const andBlueprint = BlueprintHelper.circuitToBlueprint("main", circuit);
      expect(andBlueprint).toStrictEqual(BLUEPRINT_AND);
    });

    test("returns the blueprint for a NAND chip", () => {
      const input0 = circuit.spawnInputIOChip();
      const input1 = circuit.spawnInputIOChip();
      const output0 = circuit.spawnOutputIOChip();
      const andChip = circuit.createCoreChip("AND");
      const notChip = circuit.createCoreChip("NOT");

      circuit.spawnWire(input0.pin, andChip.inputPins[0]);
      circuit.spawnWire(input1.pin, andChip.inputPins[1]);
      circuit.spawnWire(andChip.outputPins[0], notChip.inputPins[0]);
      circuit.spawnWire(notChip.outputPins[0], output0.pin);

      const nandBlueprint = BlueprintHelper.circuitToBlueprint("main", circuit);
      expect(nandBlueprint).toStrictEqual(BLUEPRINT_NAND);
    });
  });
});
