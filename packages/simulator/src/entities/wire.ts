import { entityIdService } from "../entity-id-service";
import { Entity } from "./entity";

export class Wire extends Entity {
	public readonly startPinId: string;
	public readonly endPinId: string;

	constructor(args: { startPinId: string; endPinId: string }) {
		super({
			id: entityIdService.getId(), // TODO fix
			type: "wire",
		});

		this.startPinId = args.startPinId;
		this.endPinId = args.endPinId;
	}
}
