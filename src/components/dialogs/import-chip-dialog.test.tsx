import * as React from "react";
import "@testing-library/jest-dom";
import { fireEvent, render, screen } from "@testing-library/react";
import { ImportChipDialog } from "./import-chip-dialog";

const CHIP_NAME_INPUT = "importChipNameInput";
const CHIP_BLUEPRINT_INPUT = "importChipBlueprintInput";
const CANCEL_BUTTON = "importChipCancelButton";
const CONFIRM_INPUT = "importChipConfirmButton";

const onConfirm = jest.fn();
const onDismiss = jest.fn();

describe("ImportChipDialog", () => {
  beforeEach(() => {
    render(<ImportChipDialog onConfirm={onConfirm} onDismiss={onDismiss} />);
  });

  afterEach(() => {
    jest.resetAllMocks();
  });

  test("chip name input box is visible", async () => {
    expect(screen.getByTestId(CHIP_NAME_INPUT)).toBeVisible();
  });

  test("chip blueprint input box is visible", async () => {
    expect(screen.getByTestId(CHIP_BLUEPRINT_INPUT)).toBeVisible();
  });

  test("does not call onConfirm if chip name is empty", async () => {
    fireEvent.change(screen.getByTestId(CHIP_BLUEPRINT_INPUT), {
      target: { value: `{main: testBlueprint}` },
    });
    fireEvent.click(screen.getByTestId(CONFIRM_INPUT));
    expect(onConfirm).not.toBeCalled();
  });

  test("does not call onConfirm if blueprint value is empty", async () => {
    fireEvent.change(screen.getByTestId(CHIP_NAME_INPUT), {
      target: { value: "NOR" },
    });
    fireEvent.click(screen.getByTestId(CONFIRM_INPUT));
    expect(onConfirm).not.toBeCalled();
  });

  test("calls onConfirm only if chip name and blueprint value are not empty", async () => {
    fireEvent.change(screen.getByTestId(CHIP_NAME_INPUT), {
      target: { value: "NOR" },
    });
    fireEvent.change(screen.getByTestId(CHIP_BLUEPRINT_INPUT), {
      target: { value: `{main: testBlueprint}` },
    });
    fireEvent.click(screen.getByTestId(CONFIRM_INPUT));
    expect(onConfirm).toBeCalled();
  });

  test("calls onDismiss when cancel button is clicked", async () => {
    fireEvent.click(screen.getByTestId(CANCEL_BUTTON));
    expect(onDismiss).toBeCalled();
  });
});
