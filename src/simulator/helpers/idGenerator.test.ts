import { idGenerator } from "./idGenerator";

describe("IdGenerator", () => {
  beforeEach(() => {
    idGenerator.reset();
  });

  test("returns correct value for the first chip", () => {
    expect(idGenerator.chipId("firstChip")).toBe("chip.firstChip.0");
  });

  test("returns correct value for the first input chip", () => {
    expect(idGenerator.inputChipId()).toBe("chip.input.0");
  });

  test("returns correct value for the first output chip", () => {
    expect(idGenerator.outputChipId()).toBe("chip.output.0");
  });

  test("returns correct value for the second chip", () => {
    idGenerator.chipId("firstChip");
    expect(idGenerator.chipId("secondChip")).toBe("chip.secondChip.1");
  });

  test("returns correct value for the second input chip", () => {
    idGenerator.chipId("firstChip");
    expect(idGenerator.inputChipId()).toBe("chip.input.1");
  });

  test("returns correct value for the third output chip", () => {
    idGenerator.chipId("firstChip");
    idGenerator.inputChipId();
    expect(idGenerator.outputChipId()).toBe("chip.output.2");
  });
});
