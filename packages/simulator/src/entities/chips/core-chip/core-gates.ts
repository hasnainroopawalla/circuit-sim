import { type Pin, PinState } from "../../pin";

export type ICoreGate = "AND" | "OR" | "NOT";

export const CORE_GATES = {
	AND: {
		inputPins: 2,
		outputPins: 1,
		action: (inputPins: Pin[]) => [inputPins[0].state && inputPins[1].state],
		color: "#ff7f50",
	},
	OR: {
		inputPins: 2,
		outputPins: 1,
		action: (inputPins: Pin[]) => [inputPins[0].state || inputPins[1].state],
		color: "#008000",
	},
	NOT: {
		inputPins: 1,
		outputPins: 1,
		action: (inputPins: Pin[]) => [
			inputPins[0].state === PinState.High ? PinState.Low : PinState.High,
		],
		color: "#a20f52",
	},
};
