import p5 from "p5";
import { BaseMixin } from "ts-mxn";
import { ICircuitBoard } from "../../circuit-board.interface";
import { EventData, pubsub } from "@circuit-sim/pubsub";
import { CircuitChip } from "../../../chips";
import {
  blueprintToCircuitBoard,
  circuitBoardToBlueprint,
} from "./blueprint-service-utils";

export type IBlueprintService = {
  saveCircuit: (eventData: EventData["SaveCircuit"]) => void;
  loadCircuit: (name: string, blueprint: string, color: string) => CircuitChip;
};

type IBlueprintServiceArgs = {
  p: p5;
  circuitBoard: ICircuitBoard;
};

class BlueprintService implements IBlueprintService {
  private p: p5;
  private circuitBoard: ICircuitBoard;

  constructor(args: IBlueprintServiceArgs) {
    this.circuitBoard = args.circuitBoard;
    this.p = args.p;
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

  public loadCircuit(
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

type IBlueprintMixinArgs = Omit<IBlueprintServiceArgs, "circuitBoard">;

export class BlueprintMixin extends BaseMixin<
  ICircuitBoard,
  IBlueprintService
> {
  constructor(args: IBlueprintMixinArgs) {
    super({
      methods: ["loadCircuit", "saveCircuit"],
      props: [],
      initMixin: circuitBoard =>
        new BlueprintService({ circuitBoard, ...args }),
    });
  }
}
