import {
	ChipType,
	IOChipType,
	type AtomicChip,
	type Chip,
	type CompositeChip,
	type InputChip,
	type IOChip,
	type OutputChip,
} from "./chips";
import { EntityType, type Entity } from "./entity";
import type { Pin } from "./pin";

export const EntityUtils = {
	isPin: (entity: Entity): entity is Pin => {
		return entity.entityType === EntityType.Pin;
	},

	isChip: (entity: Entity): entity is Chip => {
		return entity.entityType === EntityType.Chip;
	},

	isIOChip: (entity: Entity): entity is IOChip => {
		return EntityUtils.isChip(entity) && entity.chipType === ChipType.IO;
	},

	isInputChip: (entity: Entity): entity is InputChip => {
		return (
			EntityUtils.isIOChip(entity) && entity.ioChipType === IOChipType.Input
		);
	},

	isOutputChip: (entity: Entity): entity is OutputChip => {
		return (
			EntityUtils.isIOChip(entity) && entity.ioChipType === IOChipType.Output
		);
	},

	isAtomicChip: (entity: Entity): entity is AtomicChip => {
		return EntityUtils.isChip(entity) && entity.chipType === ChipType.Atomic;
	},

	isCompositeChip: (entity: Entity): entity is CompositeChip => {
		return EntityUtils.isChip(entity) && entity.chipType === ChipType.Composite;
	},
};
