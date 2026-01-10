export class PinLengthMismatchError extends Error {
	readonly kind = "PinLengthMismatch";

	constructor(numInputPins: number, numOutputPins: number) {
		super(
			`Pin lengths dont match: [numInputPins=${numInputPins}, numOutputPins=${numOutputPins}]`,
		);
	}
}
