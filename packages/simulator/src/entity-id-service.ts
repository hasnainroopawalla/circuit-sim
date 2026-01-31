import type { PinType } from "./entities/pin";

const ENTITY_ID_START_INDEX = -1;

class EntityIdService {
	current: number;

	constructor() {
		this.current = ENTITY_ID_START_INDEX;
	}

	public reset(): void {
		this.current = ENTITY_ID_START_INDEX;
	}

	public generateId(): string {
		this.current += 1;
		return this.current.toString();
	}

	public generatePinId(
		chipId: string,
		chipPinIndex: number,
		pinType: PinType,
	): string {
		return `${chipId}.${pinType}.${chipPinIndex}`;
	}

	public parsePinId(
		pinId: string,
	): [chipId: string, pinType: PinType, chipPinId: string] {
		const [chipId, pinType, chipPinId] = pinId.split(".");
		return [chipId, pinType as PinType, chipPinId];
	}
}

// Export a singleton instance of the service
export const entityIdService = new EntityIdService();
