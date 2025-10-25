import { fireEvent, render, screen } from "@testing-library/react";
import { ImportChipDialog } from "./import-chip-dialog";

const CHIP_NAME_INPUT = "import-chip-name-input";
const CHIP_BLUEPRINT_INPUT = "import-chip-blueprint-input";
const CONFIRM_INPUT = "importChipConfirmButton";

const onConfirm = jest.fn();

describe("ImportChipDialog", () => {
	beforeEach(() => {
		render(<ImportChipDialog onConfirm={onConfirm} />);
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
		expect(onConfirm).not.toHaveBeenCalled();
	});

	test("does not call onConfirm if blueprint value is empty", async () => {
		fireEvent.change(screen.getByTestId(CHIP_NAME_INPUT), {
			target: { value: "NOR" },
		});
		fireEvent.click(screen.getByTestId(CONFIRM_INPUT));
		expect(onConfirm).not.toHaveBeenCalled();
	});

	test("calls onConfirm only if chip name and blueprint value are not empty", async () => {
		fireEvent.change(screen.getByTestId(CHIP_NAME_INPUT), {
			target: { value: "NOR" },
		});
		fireEvent.change(screen.getByTestId(CHIP_BLUEPRINT_INPUT), {
			target: { value: `{main: testBlueprint}` },
		});
		fireEvent.click(screen.getByTestId(CONFIRM_INPUT));
		expect(onConfirm).toHaveBeenCalled();
	});
});
