import CircuitHelper from "./circuitHelper";

describe("computeReferencePoint", () => {
  test("returns correct value", () => {
    expect(
      CircuitHelper.computeReferencePoint({ x: 7, y: 4 }, { x: 2, y: 3 }, 0.8)
    ).toStrictEqual({
      x: 6,
      y: 3.8,
    });
  });
});
