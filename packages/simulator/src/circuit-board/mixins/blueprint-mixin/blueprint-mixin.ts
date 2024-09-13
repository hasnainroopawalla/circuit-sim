import p5 from "p5";
import { BaseMixin } from "power-mixin";
import { ICircuitBoard } from "../../circuit-board.interface";
import { EventData, pubsub } from "@circuit-sim/pubsub";
import { CircuitChip } from "../../../chips";
import {
  blueprintToCircuitBoard,
  circuitBoardToBlueprint,
} from "./blueprint-service-utils";

export type IBlueprintService = {
  saveCircuit: (eventData: EventData["SaveCircuit"]) => void;
  // TODO: rename to loadCircuit
  createCircuitChipFromBlueprint: (
    name: string,
    blueprint: string,
    color: string
  ) => CircuitChip;
};

class BlueprintService implements IBlueprintService {
  private p: p5;
  private circuitBoard: ICircuitBoard;

  constructor(circuitBoard: ICircuitBoard, p: p5) {
    this.circuitBoard = circuitBoard;
    this.p = p;
  }

  public saveCircuit(eventData: EventData["SaveCircuit"]): void {
    // Create the circuit chip only if inputs and outputs exist
    if (
      this.circuitBoard.entities.inputs.length === 0 ||
      this.circuitBoard.entities.outputs.length === 0
    ) {
      pubsub.publish("Notification", {
        text: "Circuit chip not created due to missing inputs/outputs",
      });
    }

    const blueprint = circuitBoardToBlueprint("main", this.circuitBoard);

    pubsub.publish("AddCircuitChipToToolbar", {
      name: eventData.name,
      blueprint: JSON.stringify(blueprint),
    });

    this.circuitBoard.initEntities();
  }

  public createCircuitChipFromBlueprint(
    name: string,
    blueprint: string,
    color: string
  ): CircuitChip {
    const chipCircuitBoard = blueprintToCircuitBoard(
      this.p,
      name,
      blueprint,
      "main"
    );

    return this.circuitBoard.createCircuitChip(chipCircuitBoard, color, false);
  }
}

export class BlueprintMixin extends BaseMixin<
  ICircuitBoard,
  IBlueprintService
> {
  constructor(p: p5) {
    super({
      methods: [],
      props: [],
      initMixin: circuitBoard => new BlueprintService(circuitBoard, p),
    });
  }
}
