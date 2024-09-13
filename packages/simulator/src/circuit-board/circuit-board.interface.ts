import type {
  ICircuitBoardState,
  ICore,
  IEntityManager,
  IMouseInputManager,
  IRenderer,
} from "./mixins";

export type ICircuitBoard = ICircuitBoardState &
  IRenderer &
  IMouseInputManager &
  IEntityManager &
  ICore;
