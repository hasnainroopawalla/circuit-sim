import p5 from "p5";
import { EventData, emitter } from "@circuit-sim/events";
import type { CircuitBoard } from "../../circuit-board";
import {
  blueprintToCircuitBoard,
  circuitBoardToBlueprint,
} from "./blueprint-service-utils";
import type { CircuitChip } from "../../../chips";

export class BlueprintService {
  p: p5;
  circuitBoard: CircuitBoard;

  constructor(p: p5, circuitBoard: CircuitBoard) {
    this.p = p;
    this.circuitBoard = circuitBoard;
  }

  public saveCircuit(eventData: EventData["SaveCircuit"]): void {
    // Create the circuit chip only if inputs and outputs exist
    if (
      this.circuitBoard.entities.inputs.length === 0 ||
      this.circuitBoard.entities.outputs.length === 0
    ) {
      emitter.emit("Notification", {
        text: "Circuit chip not created due to missing inputs/outputs",
      });
    }

    const blueprint = circuitBoardToBlueprint("main", this.circuitBoard);

    emitter.emit("AddCircuitChipToToolbar", {
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
