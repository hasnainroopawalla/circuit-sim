import p5 from "p5";
import { mix } from "power-mixin";
import { IStateManager, StateManagerMixin } from "./mixins/state-manager-mixin";
import { IRenderer, RendererMixin } from "./mixins/renderer-mixin";
import type { Position, Size } from "../common";
import {
  IMouseInputManager,
  MouseInputManagerMixin,
} from "./mixins/mouse-input-mixin";
import {
  EntityManagerMixin,
  IEntityManager,
} from "./mixins/entity-manager-mixin";

export type ICircuitBoard = IStateManager &
  IRenderer &
  IMouseInputManager &
  IEntityManager;

export const createCircuitBoard = (args: {
  p: p5;
  name: string;
  options: {
    position: Position;
    size: Size<"rect">;
  };
  isCircuitChip?: boolean;
}) => {
  const circuitBoard = mix<ICircuitBoard>({
    mixins: [
      new StateManagerMixin(args.p),
      // TODO: improve arg types
      new RendererMixin(
        args.p,
        args.name,
        args.options.position,
        args.options.size
      ),
      new MouseInputManagerMixin(args.p),
      new EntityManagerMixin(args.p),
    ],
  });

  return circuitBoard;
};
