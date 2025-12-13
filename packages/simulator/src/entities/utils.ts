import type {
	AtomicChip,
	Chip,
	CompositeChip,
	InputChip,
	IOChip,
	OutputChip,
} from "./chips";
import type { Entity } from "./entity";
import type { Pin } from "./pin";

export const EntityUtils = {
	isPin: (entity: Entity): entity is Pin => {
		return entity.entityType === "pin";
	},

	isChip: (entity: Entity): entity is Chip => {
		return entity.entityType === "chip";
	},

	isIOChip: (entity: Entity): entity is IOChip => {
		return EntityUtils.isChip(entity) && entity.chipType === "io";
	},

	isInputChip: (entity: Entity): entity is InputChip => {
		return EntityUtils.isIOChip(entity) && entity.ioChipType === "input";
	},

	isOutputChip: (entity: Entity): entity is OutputChip => {
		return EntityUtils.isIOChip(entity) && entity.ioChipType === "output";
	},

	isAtomicChip: (entity: Entity): entity is AtomicChip => {
		return EntityUtils.isChip(entity) && entity.chipType === "atomic";
	},

	isCompositeChip: (entity: Entity): entity is CompositeChip => {
		return EntityUtils.isChip(entity) && entity.chipType === "atomic";
	},
};
