import * as React from "react";
import "@testing-library/jest-dom";
import { render, screen } from "@testing-library/react";
import { Button } from "./button";

const BUTTON_TID = "buttonTestId";
const onClick = jest.fn();

describe("Button", () => {
  beforeEach(() => {
    render(
      <Button
        text="buttonText"
        dataTestId={BUTTON_TID}
        onClick={onClick}
        size="small"
      />
    );
  });

  afterEach(() => {
    jest.resetAllMocks();
  });

  test("button with text is visible", async () => {
    const button = screen.getByTestId(BUTTON_TID);
    expect(button.textContent).toBe("buttonText");
    expect(button).toBeVisible();
  });
});
