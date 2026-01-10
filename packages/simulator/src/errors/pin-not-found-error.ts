export class PinNotFoundError extends Error {
	readonly kind = "PinNotFound";

	constructor(chipId: string, chipName: string, pinName: string) {
		super(
			`Pin "${pinName}" with id "${chipId}" not found on chip "${chipName}"`,
		);
	}
}
