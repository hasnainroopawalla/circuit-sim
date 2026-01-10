import type { ChipType } from "../entities/chips";

export class ChipNotFoundError extends Error {
	readonly kind = "ChipNotFound";

	constructor(chipKind: ChipType, chipName: string) {
		super(`"${chipKind}" chip "${chipName}" not found in library`);
	}
}
