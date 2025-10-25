import type { Chip, IOChip } from "../chips";
import type { Wire } from "../wire";
import type {
	IBlueprintService,
	ICoreService,
	IEntityService,
	IExternalEventsService,
	IMouseInputService,
	IRenderService,
	IStateService,
} from "./mixins";

export type ICircuitBoard = IStateService &
	IRenderService &
	IMouseInputService &
	IEntityService &
	ICoreService &
	IExternalEventsService &
	IBlueprintService;

export enum MouseInput {
	Click = "Click",
	DoubleClick = "DoubleClick",
	Drag = "Drag",
	Move = "Move",
}

export enum State {
	Idle = "Idle",
	Reposition = "Reposition",
	Wiring = "Wiring",
	SpawnChip = "SpawnChip",
	SpawnIOChip = "SpawnIOChip",
}

export type CircuitBoardEntities = {
	inputs: IOChip[];
	outputs: IOChip[];
	wires: Wire[];
	chips: Chip[];
};
