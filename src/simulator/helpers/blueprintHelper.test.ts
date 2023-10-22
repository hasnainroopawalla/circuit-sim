import p5 from "p5";
import BlueprintHelper from "./blueprintHelper";
import { Circuit } from "../circuit";
import { idGenerator } from "./idGenerator";

const sketch = (p: p5) => {
  p.setup = () => {};
  p.draw = () => {};
};

const SCHEMA_AND = {
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

const SCHEMA_NAND = {
  main: {
    inputs: [
      { id: "chip.input.0" },
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

const SCHEMA_AND_USING_NOR = {
  NOR: {
    inputs: [{ id: "chip.NOR.7.input.0" }, { id: "chip.NOR.7.input.1" }],
    outputs: [{ id: "chip.NOR.7.output.0" }],
    chips: [
      { id: "chip.OR.3", name: "OR" },
      { id: "chip.NOT.4", name: "NOT" },
    ],
    wires: [
      ["chip.NOR.7.input.0/input.0", "chip.OR.3/input.0"],
      ["chip.NOR.7.input.1/input.0", "chip.OR.3/input.1"],
      ["chip.OR.3/output.0", "chip.NOT.4/input.0"],
      ["chip.NOT.4/output.0", "chip.NOR.7.output.0/output.0"],
    ],
  },
  main: {
    inputs: [{ id: "chip.input.8" }, { id: "chip.input.9" }],
    outputs: [{ id: "chip.output.10" }],
    chips: [
      { id: "chip.NOR.5", name: "NOR" },
      { id: "chip.NOR.6", name: "NOR" },
      { id: "chip.NOR.7", name: "NOR" },
    ],
    wires: [
      ["chip.input.8/output.0", "chip.NOR.7.input.0/input.0"],
      ["chip.input.8/output.0", "chip.NOR.7.input.1/input.0"],
      ["chip.input.9/output.0", "chip.NOR.7.input.0/input.0"],
      ["chip.input.9/output.0", "chip.NOR.7.input.1/input.0"],
      ["chip.NOR.7.output.0/output.0", "chip.NOR.7.input.0/input.0"],
      ["chip.NOR.7.output.0/output.0", "chip.NOR.7.input.1/input.0"],
      ["chip.NOR.7.output.0/output.0", "chip.output.10/input.0"],
    ],
  },
};

const SCHEMA_DATA_LATCH = {
  NOR: {
    inputs: [
      {
        id: "chip.NOR.16.input.0",
      },
      {
        id: "chip.NOR.16.input.1",
      },
    ],
    outputs: [
      {
        id: "chip.NOR.16.output.0",
      },
    ],
    chips: [
      {
        id: "chip.OR.14",
        name: "OR",
      },
      {
        id: "chip.NOT.15",
        name: "NOT",
      },
    ],
    wires: [
      ["chip.OR.14/output.0", "chip.NOT.15/input.0"],
      ["chip.NOR.16.input.0/input.0", "chip.OR.14/input.0"],
      ["chip.NOR.16.input.1/input.0", "chip.OR.14/input.1"],
      ["chip.NOT.15/output.0", "chip.NOR.16.output.0/output.0"],
    ],
  },
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
        id: "chip.AND.3", //changed
        name: "AND",
      },
      {
        id: "chip.AND.4",
        name: "AND",
      },
      {
        id: "chip.NOT.5",
        name: "NOT",
      },
      {
        id: "chip.NOR.11",
        name: "NOR",
      },
      {
        id: "chip.NOR.17",
        name: "NOR",
      },
    ],
    wires: [
      ["chip.NOR.17.output.0/output.0", "chip.output.2/input.0"],
      ["chip.NOR.11.output.0/output.0", "chip.NOR.17.input.0/input.0"],
      ["chip.NOR.17.output.0/output.0", "chip.NOR.11.input.1/input.0"],
      ["chip.AND.3/output.0", "chip.NOR.11.input.0/input.0"], // start
      ["chip.AND.4/output.0", "chip.NOR.17.input.1/input.0"],
      ["chip.NOT.5/output.0", "chip.AND.4/input.1"],
      ["chip.input.1/output.0", "chip.AND.4/input.0"],
      ["chip.input.1/output.0", "chip.AND.3/input.1"], // end
      ["chip.input.0/output.0", "chip.NOT.5/input.0"],
      ["chip.input.0/output.0", "chip.AND.3/input.0"], //end
    ],
  },
};

describe("BlueprintHelper", () => {
  let p: p5;

  beforeEach(() => {
    p = new p5(sketch);
    idGenerator.reset();
  });

  const createCircuit = (name: string = "main"): Circuit => {
    return new Circuit(
      p,
      name,
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
  };

  const createANDCircuit = (): Circuit => {
    const circuit = createCircuit("AND");
    const input0 = circuit.spawnInputIOChip();
    const input1 = circuit.spawnInputIOChip();
    const output0 = circuit.spawnOutputIOChip();
    const andChip = circuit.createCoreChip("AND");

    circuit.spawnWire(input0.pin, andChip.inputPins[0]);
    circuit.spawnWire(input1.pin, andChip.inputPins[1]);
    circuit.spawnWire(andChip.outputPins[0], output0.pin);
    return circuit;
  };

  const createNANDCircuit = (): Circuit => {
    const circuit = createCircuit("NAND");
    const input0 = circuit.spawnInputIOChip();
    const input1 = circuit.spawnInputIOChip();
    const output0 = circuit.spawnOutputIOChip();
    const andChip = circuit.createCoreChip("AND");
    const notChip = circuit.createCoreChip("NOT");

    circuit.spawnWire(input0.pin, andChip.inputPins[0]);
    circuit.spawnWire(input1.pin, andChip.inputPins[1]);
    circuit.spawnWire(andChip.outputPins[0], notChip.inputPins[0]);
    circuit.spawnWire(notChip.outputPins[0], output0.pin);
    return circuit;
  };

  const createNORCircuit = (): Circuit => {
    const circuit = createCircuit("NOR");
    const input0 = circuit.spawnInputIOChip();
    const input1 = circuit.spawnInputIOChip();
    const output0 = circuit.spawnOutputIOChip();
    const orChip = circuit.createCoreChip("OR");
    const notChip = circuit.createCoreChip("NOT");

    circuit.spawnWire(input0.pin, orChip.inputPins[0]);
    circuit.spawnWire(input1.pin, orChip.inputPins[1]);
    circuit.spawnWire(orChip.outputPins[0], notChip.inputPins[0]);
    circuit.spawnWire(notChip.outputPins[0], output0.pin);
    return circuit;
  };

  describe("circuitToBlueprint", () => {
    test("returns the blueprint for a custom AND chip", () => {
      const circuit = createANDCircuit();
      const andBlueprint = BlueprintHelper.circuitToBlueprint("main", circuit);
      expect(andBlueprint).toStrictEqual(SCHEMA_AND);
    });

    test("returns the blueprint for a NAND chip", () => {
      const circuit = createNANDCircuit();
      const nandBlueprint = BlueprintHelper.circuitToBlueprint("main", circuit);
      expect(nandBlueprint).toStrictEqual(SCHEMA_NAND);
    });

    test("returns the blueprint for an AND gate using 3 NOR chips", () => {
      const baseCircuit = createCircuit();

      const norCircuit = createNORCircuit();
      const nor0 = baseCircuit.createCustomChip(norCircuit);
      const nor1 = baseCircuit.createCustomChip(norCircuit);
      const nor2 = baseCircuit.createCustomChip(norCircuit);

      const input0 = baseCircuit.spawnInputIOChip();
      const input1 = baseCircuit.spawnInputIOChip();
      const output1 = baseCircuit.spawnOutputIOChip();

      baseCircuit.spawnWire(input0.pin, nor0.inputPins[0]);
      baseCircuit.spawnWire(input0.pin, nor0.inputPins[1]);
      baseCircuit.spawnWire(input1.pin, nor1.inputPins[0]);
      baseCircuit.spawnWire(input1.pin, nor1.inputPins[1]);
      baseCircuit.spawnWire(nor0.outputPins[0], nor2.inputPins[0]);
      baseCircuit.spawnWire(nor1.outputPins[0], nor2.inputPins[1]);
      baseCircuit.spawnWire(nor2.outputPins[0], output1.pin);

      const andUsingNorBlueprint = BlueprintHelper.circuitToBlueprint(
        "main",
        baseCircuit
      );
      expect(andUsingNorBlueprint).toStrictEqual(SCHEMA_AND_USING_NOR);
    });
  });

  describe("blueprintToCircuit", () => {
    test("returns the blueprint for a custom AND chip", () => {
      const andBlueprint = `{"main":{"inputs":[{"id":"chip.input.0"},{"id":"chip.input.1"}],"outputs":[{"id":"chip.output.2"}],"chips":[{"id":"chip.AND.3","name":"AND"}],"wires":[["chip.input.0/output.0","chip.AND.3/input.0"],["chip.input.1/output.0","chip.AND.3/input.1"],["chip.AND.3/output.0","chip.output.2/input.0"]]}}`;
      const andCircuit = BlueprintHelper.blueprintToCircuit(
        p,
        "AND",
        andBlueprint,
        "main"
      );
      expect(andCircuit.inputs.length).toBe(SCHEMA_AND.main.inputs.length);
      for (let i = 0; i < andCircuit.inputs.length; i++) {
        expect(andCircuit.inputs[i].id).toBe(SCHEMA_AND.main.inputs[i].id);
      }

      expect(andCircuit.outputs.length).toBe(SCHEMA_AND.main.outputs.length);
      for (let i = 0; i < andCircuit.outputs.length; i++) {
        expect(andCircuit.outputs[i].id).toBe(SCHEMA_AND.main.outputs[i].id);
      }

      expect(andCircuit.chips.length).toBe(SCHEMA_AND.main.chips.length);
      for (let i = 0; i < andCircuit.chips.length; i++) {
        expect(andCircuit.chips[i].id).toBe(SCHEMA_AND.main.chips[i].id);
        expect(andCircuit.chips[i].name).toBe(SCHEMA_AND.main.chips[i].name);
      }

      expect(andCircuit.wires.length).toBe(SCHEMA_AND.main.wires.length);
      for (let i = 0; i < andCircuit.wires.length; i++) {
        const wire = andCircuit.wires[i];
        const wireStart = `${wire.startPin.chip.id}/${
          wire.startPin.isInput ? "input" : "output"
        }.${wire.startPin.id}`;
        const wireEnd = `${wire.endPin.chip.id}/${
          wire.endPin.isInput ? "input" : "output"
        }.${wire.endPin.id}`;

        expect(wireStart).toBe(SCHEMA_AND.main.wires[i][0]);
        expect(wireEnd).toBe(SCHEMA_AND.main.wires[i][1]);
      }
    });

    test("returns the blueprint for a NAND chip", () => {
      const nandBlueprint = `{"main":{"inputs":[{"id":"chip.input.0"},{"id":"chip.input.1"}],"outputs":[{"id":"chip.output.2"}],"chips":[{"id":"chip.AND.3","name":"AND"},{"id":"chip.NOT.4","name":"NOT"}],"wires":[["chip.input.0/output.0","chip.AND.3/input.0"],["chip.input.1/output.0","chip.AND.3/input.1"],["chip.AND.3/output.0","chip.NOT.4/input.0"],["chip.NOT.4/output.0","chip.output.2/input.0"]]}}`;
      const nandCircuit = BlueprintHelper.blueprintToCircuit(
        p,
        "NAND",
        nandBlueprint,
        "main"
      );

      expect(nandCircuit.inputs.length).toBe(SCHEMA_NAND.main.inputs.length);
      for (let i = 0; i < nandCircuit.inputs.length; i++) {
        expect(nandCircuit.inputs[i].id).toBe(SCHEMA_NAND.main.inputs[i].id);
      }

      expect(nandCircuit.outputs.length).toBe(SCHEMA_NAND.main.outputs.length);
      for (let i = 0; i < nandCircuit.outputs.length; i++) {
        expect(nandCircuit.outputs[i].id).toBe(SCHEMA_NAND.main.outputs[i].id);
      }

      expect(nandCircuit.chips.length).toBe(SCHEMA_NAND.main.chips.length);
      for (let i = 0; i < nandCircuit.chips.length; i++) {
        expect(nandCircuit.chips[i].id).toBe(SCHEMA_NAND.main.chips[i].id);
        expect(nandCircuit.chips[i].name).toBe(SCHEMA_NAND.main.chips[i].name);
      }

      expect(nandCircuit.wires.length).toBe(SCHEMA_NAND.main.wires.length);
      for (let i = 0; i < nandCircuit.wires.length; i++) {
        const wire = nandCircuit.wires[i];

        const wireStart = `${wire.startPin.chip.id}/${
          wire.startPin.isInput ? "input" : "output"
        }.${wire.startPin.id}`;
        const wireEnd = `${wire.endPin.chip.id}/${
          wire.endPin.isInput ? "input" : "output"
        }.${wire.endPin.id}`;

        expect(wireStart).toBe(SCHEMA_NAND.main.wires[i][0]);
        expect(wireEnd).toBe(SCHEMA_NAND.main.wires[i][1]);
      }
    });

    test("returns the blueprint for a Data Latch", () => {
      const dataLatchBlueprint = `{"NOR":{"inputs":[{"id":"chip.NOR.16.input.0"},{"id":"chip.NOR.16.input.1"}],"outputs":[{"id":"chip.NOR.16.output.0"}],"chips":[{"id":"chip.OR.14","name":"OR"},{"id":"chip.NOT.15","name":"NOT"}],"wires":[["chip.OR.14/output.0","chip.NOT.15/input.0"],["chip.NOR.16.input.0/input.0","chip.OR.14/input.0"],["chip.NOR.16.input.1/input.0","chip.OR.14/input.1"],["chip.NOT.15/output.0","chip.NOR.16.output.0/output.0"]]},"main":{"inputs":[{"id":"chip.input.0"},{"id":"chip.input.1"}],"outputs":[{"id":"chip.output.17"}],"chips":[{"id":"chip.AND.2","name":"AND"},{"id":"chip.AND.3","name":"AND"},{"id":"chip.NOT.4","name":"NOT"},{"id":"chip.NOR.10","name":"NOR"},{"id":"chip.NOR.16","name":"NOR"}],"wires":[["chip.NOR.16.output.0/output.0","chip.output.17/input.0"],["chip.NOR.10.output.0/output.0","chip.NOR.16.input.0/input.0"],["chip.NOR.16.output.0/output.0","chip.NOR.10.input.1/input.0"],["chip.AND.2/output.0","chip.NOR.10.input.0/input.0"],["chip.AND.3/output.0","chip.NOR.16.input.1/input.0"],["chip.NOT.4/output.0","chip.AND.3/input.1"],["chip.input.1/output.0","chip.AND.3/input.0"],["chip.input.1/output.0","chip.AND.2/input.1"],["chip.input.0/output.0","chip.NOT.4/input.0"],["chip.input.0/output.0","chip.AND.2/input.0"]]}}`;
      const dataLatchCircuit = BlueprintHelper.blueprintToCircuit(
        p,
        "D-Latch",
        dataLatchBlueprint,
        "main"
      );

      expect(dataLatchCircuit.inputs.length).toBe(
        SCHEMA_DATA_LATCH.main.inputs.length
      );
      for (let i = 0; i < dataLatchCircuit.inputs.length; i++) {
        expect(dataLatchCircuit.inputs[i].id).toBe(
          SCHEMA_DATA_LATCH.main.inputs[i].id
        );
      }

      expect(dataLatchCircuit.outputs.length).toBe(
        SCHEMA_DATA_LATCH.main.outputs.length
      );
      for (let i = 0; i < dataLatchCircuit.outputs.length; i++) {
        expect(dataLatchCircuit.outputs[i].id).toBe(
          SCHEMA_DATA_LATCH.main.outputs[i].id
        );
      }

      expect(dataLatchCircuit.chips.length).toBe(
        SCHEMA_DATA_LATCH.main.chips.length
      );
      for (let i = 0; i < dataLatchCircuit.chips.length; i++) {
        expect(dataLatchCircuit.chips[i].id).toBe(
          SCHEMA_DATA_LATCH.main.chips[i].id
        );
        expect(dataLatchCircuit.chips[i].name).toBe(
          SCHEMA_DATA_LATCH.main.chips[i].name
        );
      }

      expect(dataLatchCircuit.wires.length).toBe(
        SCHEMA_DATA_LATCH.main.wires.length
      );
      for (let i = 0; i < dataLatchCircuit.wires.length; i++) {
        const wire = dataLatchCircuit.wires[i];

        const wireStart = `${wire.startPin.chip.id}/${
          wire.startPin.isInput ? "input" : "output"
        }.${wire.startPin.id}`;
        const wireEnd = `${wire.endPin.chip.id}/${
          wire.endPin.isInput ? "input" : "output"
        }.${wire.endPin.id}`;

        expect(wireStart).toBe(SCHEMA_DATA_LATCH.main.wires[i][0]);
        expect(wireEnd).toBe(SCHEMA_DATA_LATCH.main.wires[i][1]);
      }
    });
  });
});
