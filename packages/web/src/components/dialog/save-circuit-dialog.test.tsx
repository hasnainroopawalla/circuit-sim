import * as React from "react";
import { fireEvent, render, screen } from "@testing-library/react";
import { SaveCircuitDialog } from "./save-circuit-dialog";

const CHIP_NAME_INPUT = "saveCircuitChipNameInput";
const CANCEL_BUTTON = "saveCircuitCancelButton";
const CONFIRM_INPUT = "saveCircuitConfirmButton";

const onConfirm = jest.fn();
const onDismiss = jest.fn();

describe("ImportChipDialog", () => {
  beforeEach(() => {
    render(<SaveCircuitDialog onConfirm={onConfirm} />);
  });

  afterEach(() => {
    jest.resetAllMocks();
  });

  test("chip name input box is visible", async () => {
    expect(screen.getByTestId(CHIP_NAME_INPUT)).toBeVisible();
  });

  test("does not call onConfirm if chip name is empty", async () => {
    fireEvent.click(screen.getByTestId(CONFIRM_INPUT));
    expect(onConfirm).not.toHaveBeenCalled();
  });

  test("calls onConfirm only if chip name is not empty", async () => {
    fireEvent.change(screen.getByTestId(CHIP_NAME_INPUT), {
      target: { value: "NOR" },
    });
    fireEvent.click(screen.getByTestId(CONFIRM_INPUT));
    expect(onConfirm).toHaveBeenCalled();
  });

  test("calls onDismiss when cancel button is clicked", async () => {
    fireEvent.click(screen.getByTestId(CANCEL_BUTTON));
    expect(onDismiss).toHaveBeenCalled();
  });
});
