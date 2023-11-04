import * as React from "react";
import "@testing-library/jest-dom";
import { fireEvent, render, screen } from "@testing-library/react";
import { UpperToolbar } from "./upper-toolbar";

const saveButtonOnClick = jest.fn();
const optionsButtonOnClick = jest.fn();

describe("UpperToolbar", () => {
  beforeEach(() => {
    render(
      <UpperToolbar
        saveButtonOnClick={saveButtonOnClick}
        optionsButtonOnClick={optionsButtonOnClick}
      />
    );
  });

  afterEach(() => {
    jest.resetAllMocks();
  });

  test("save button visible", async () => {
    const saveButton = screen.getByText("SAVE");
    expect(saveButton).toBeInTheDocument();
    fireEvent.click(saveButton);
    expect(saveButtonOnClick).toBeCalled();
  });

  test("options button visible", async () => {
    const optionsButton = screen.getByText("OPTIONS");
    expect(optionsButton).toBeInTheDocument();
    fireEvent.click(optionsButton);
    expect(optionsButtonOnClick).toBeCalled();
  });
});
