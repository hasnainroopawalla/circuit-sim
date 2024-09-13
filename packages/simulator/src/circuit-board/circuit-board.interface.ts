import type { IOChip, Chip } from "../chips";
import type { Wire } from "../wire";
import type {
  IBlueprintService,
  ICircuitBoardState,
  ICore,
  IEntityManager,
  IExternalEventService,
  IMouseInputManager,
  IRenderer,
} from "./mixins";

export type ICircuitBoard = ICircuitBoardState &
  IRenderer &
  IMouseInputManager &
  IEntityManager &
  ICore &
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
