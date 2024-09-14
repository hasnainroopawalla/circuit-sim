import p5 from "p5";
import { mix } from "power-mixin";
import {
  MouseInputMixin,
  EntityMixin,
  StateMixin,
  RenderMixin,
  CoreMixin,
  ExternalEventServiceMixin,
  BlueprintMixin,
} from "./mixins";
import type { ICircuitBoard } from "./circuit-board.interface";
import { Position, Size } from "../types";

export const createCircuitBoard = (args: {
  p: p5;
  name: string;
  options: {
    position: Position;
    size: Size<"rect">;
  };
  isCircuitChip: boolean;
}) => {
  const circuitBoard = mix<ICircuitBoard>({
    mixins: [
      new CoreMixin(args.name, args.isCircuitChip),
      new StateMixin(args.p),
      // TODO: improve arg types
      new RenderMixin(args.p, args.options.position, args.options.size),
      new MouseInputMixin(args.p),
      new EntityMixin(args.p),
      new ExternalEventServiceMixin(),
      new BlueprintMixin(args.p),
    ],
  });

  return circuitBoard;
};
