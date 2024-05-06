import { entityIdService } from "./entity-id-service";

describe("EntityIdService", () => {
  beforeEach(() => {
    entityIdService.reset();
  });

  test("returns correct value for the first chip", () => {
    expect(entityIdService.chipId("firstChip")).toBe("chip.firstChip.0");
  });

  test("returns correct value for the first input chip", () => {
    expect(entityIdService.inputChipId()).toBe("chip.input.0");
  });

  test("returns correct value for the first output chip", () => {
    expect(entityIdService.outputChipId()).toBe("chip.output.0");
  });

  test("returns correct value for the second chip", () => {
    entityIdService.chipId("firstChip");
    expect(entityIdService.chipId("secondChip")).toBe("chip.secondChip.1");
  });

  test("returns correct value for the second input chip", () => {
    entityIdService.chipId("firstChip");
    expect(entityIdService.inputChipId()).toBe("chip.input.1");
  });

  test("returns correct value for the third output chip", () => {
    entityIdService.chipId("firstChip");
    entityIdService.inputChipId();
    expect(entityIdService.outputChipId()).toBe("chip.output.2");
  });
});
