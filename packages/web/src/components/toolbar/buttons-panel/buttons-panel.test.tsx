import * as React from "react";
import { render, screen } from "@testing-library/react";
import { ButtonsPanel } from "./buttons-panel";

// jest.mock("../hooks", () => ({
//   useChips: jest.fn().mockReturnValue([
//     {
//       name: "circuitChip1",
//       onClick: jest.fn(),
//     },
//   ]),
// }));

jest.mock("../../dialog", () => ({
  useDialog: jest.fn().mockReturnValue({
    openDialog: jest.fn(),
  }),
}));

// const saveButtonOnClick = jest.fn();
// const optionsButtonOnClick = jest.fn();

describe("ButtonsPanel", () => {
  beforeEach(() => {
    render(<ButtonsPanel />);
  });

  test("save button visible", async () => {
    const saveButton = screen.getByText("MENU");
    expect(saveButton).toBeInTheDocument();
    // fireEvent.click(saveButton);
    // expect(saveButtonOnClick).toHaveBeenCalled();
  });

  test("core chip buttons visible (AND, OR, NOT)", async () => {
    expect(screen.getByText("AND")).toBeInTheDocument();
    expect(screen.getByText("OR")).toBeInTheDocument();
    expect(screen.getByText("NOT")).toBeInTheDocument();
  });

  test.skip("circuit chip button visible", async () => {
    expect(screen.getByText("circuitChip1")).toBeInTheDocument();
  });
});
