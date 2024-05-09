import * as React from "react";
import "@testing-library/jest-dom";
import { fireEvent, render, screen } from "@testing-library/react";
import { LowerToolbar } from "./lower-toolbar";

jest.mock("../hooks", () => ({
  useCircuitChips: jest.fn().mockReturnValue([
    {
      name: "circuitChip1",
      onClick: jest.fn(),
    },
  ]),
}));

const importChipButtonOnClick = jest.fn();
const saveButtonOnClick = jest.fn();
const optionsButtonOnClick = jest.fn();

describe("LowerToolbar", () => {
  beforeEach(() => {
    render(
      <LowerToolbar
        saveButtonOnClick={saveButtonOnClick}
        importChipButtonOnClick={importChipButtonOnClick}
      />
    );
  });

  test("save button visible", async () => {
    const saveButton = screen.getByText("SAVE");
    expect(saveButton).toBeInTheDocument();
    fireEvent.click(saveButton);
    expect(saveButtonOnClick).toHaveBeenCalled();
  });

  test.skip("options button visible", async () => {
    const optionsButton = screen.getByText("OPTIONS");
    expect(optionsButton).toBeInTheDocument();
    fireEvent.click(optionsButton);
    expect(optionsButtonOnClick).toHaveBeenCalled();
  });

  test("core chip buttons visible (AND, OR, NOT)", async () => {
    expect(screen.getByText("AND")).toBeInTheDocument();
    expect(screen.getByText("OR")).toBeInTheDocument();
    expect(screen.getByText("NOT")).toBeInTheDocument();
  });

  test("circuit chip button visible", async () => {
    expect(screen.getByText("circuitChip1")).toBeInTheDocument();
  });

  test("import chip button visible", async () => {
    const importButton = screen.getByTestId("importChipButton");
    expect(importButton).toBeVisible();
    fireEvent.click(importButton);
    expect(importChipButtonOnClick).toHaveBeenCalled();
  });
});
