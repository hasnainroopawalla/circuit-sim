import p5 from "p5";
import { mix } from "power-mixin";
import {
  MouseInputMixin,
  EntityMixin,
  StateMixin,
  RenderMixin,
  CoreMixin,
  ExternalEventsMixin,
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
      new CoreMixin(args),
      new StateMixin(args),
      new RenderMixin(args),
      new MouseInputMixin(args),
      new EntityMixin(args),
      new ExternalEventsMixin(args),
      new BlueprintMixin(args),
    ],
  });

  return circuitBoard;
};
