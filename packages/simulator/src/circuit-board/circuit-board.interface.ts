import type { IOChip, Chip } from "../chips";
import type { Wire } from "../wire";
import type {
  IBlueprintService,
  ICoreService,
  IStateService,
  IEntityService,
  IExternalEventService,
  IMouseInputService,
  IRenderService,
} from "./mixins";

export type ICircuitBoard = IStateService &
  IRenderService &
  IMouseInputService &
  IEntityService &
  ICoreService &
  IExternalEventService &
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
