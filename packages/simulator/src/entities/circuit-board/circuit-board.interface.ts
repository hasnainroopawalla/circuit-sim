import { IOChip, Chip } from "../chips";
import { Wire } from "../wire";
import type {
  IBlueprintService,
  ICoreService,
  IStateService,
  IEntityService,
  IExternalEventsService,
  IMouseInputService,
  IRenderService,
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
