import { fireEvent, render, screen } from "@testing-library/react";
import { SaveChipDialog } from "./save-chip-dialog";

const CHIP_NAME_INPUT = "save-chip-name-input";
const CONFIRM_INPUT = "save-chip-confirm-button";

const onConfirm = jest.fn();

describe("ImportChipDialog", () => {
	beforeEach(() => {
		render(<SaveChipDialog onConfirm={onConfirm} />);
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
});
