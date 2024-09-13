import p5 from "p5";
import { mix } from "power-mixin";
import type { Position, Size } from "../common";
import {
  MouseInputMixin,
  EntityMixin,
  CircuitBoardStateMixin,
  RendererMixin,
  CoreMixin,
  ExternalEventServiceMixin,
  BlueprintMixin,
} from "./mixins";
import type { ICircuitBoard } from "./circuit-board.interface";

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
      new CoreMixin(),
      new CircuitBoardStateMixin(args.p),
      // TODO: improve arg types
      new RendererMixin(
        args.p,
        args.name,
        args.options.position,
        args.options.size
      ),
      new MouseInputMixin(args.p),
      new EntityMixin(args.p),
      new ExternalEventServiceMixin(),
      new BlueprintMixin(args.p),
    ],
  });

  return circuitBoard;
};
