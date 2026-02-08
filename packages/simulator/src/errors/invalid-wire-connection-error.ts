import type { Pin } from "../entities/pin";

export class InvalidWireConnectionError extends Error {
	readonly kind = "InvalidWireConnection";

	constructor(startPin?: Pin, endPin?: Pin) {
		super(
			`Invalid Wire Connection from "${startPin?.chip.spec.name}/${startPin?.spec.name}" to "${endPin?.chip.spec.name}/${endPin?.spec.name}"`,
		);
	}
}
