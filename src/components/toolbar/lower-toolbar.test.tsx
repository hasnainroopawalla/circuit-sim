import * as React from "react";
import "@testing-library/jest-dom";
import { fireEvent, render, screen } from "@testing-library/react";
import { LowerToolbar } from "./lower-toolbar";

const importChipButtonOnClick = jest.fn();
const customChips = [
  {
    name: "customChip1",
    onClick: jest.fn(),
  },
];

describe("LowerToolbar", () => {
  beforeEach(() => {
    render(
      <LowerToolbar
        useCustomChips={jest.fn().mockReturnValue(customChips)}
        importChipButtonOnClick={importChipButtonOnClick}
      />
    );
  });

  afterEach(() => {
    jest.resetAllMocks();
  });

  test.only("core chip buttons visible (AND, OR, NOT)", async () => {
    expect(screen.getByText("AND")).toBeInTheDocument();
    expect(screen.getByText("OR")).toBeInTheDocument();
    expect(screen.getByText("NOT")).toBeInTheDocument();
  });

  test.only("custom chip button visible", async () => {
    expect(screen.getByText("customChip1")).toBeInTheDocument();
  });

  test.only("import chip button visible", async () => {
    const importButton = screen.getByTestId("importChipButton");
    expect(importButton).toBeVisible();
    fireEvent.click(importButton);
    expect(importChipButtonOnClick).toBeCalled();
  });
});
