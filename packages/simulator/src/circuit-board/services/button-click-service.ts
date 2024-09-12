import p5 from "p5";
import { Chip } from "../../chips";
import type { CircuitBoard } from "../circuit-board";
import { Mode } from "../circuit-board.interface";
import { EventData, pubsub } from "@circuit-sim/pubsub";

export class ButtonClickService {
  p: p5;
  circuitBoard: CircuitBoard;

  constructor(p: p5, circuitBoard: CircuitBoard) {
    this.p = p;
    this.circuitBoard = circuitBoard;
    !this.circuitBoard.isCircuitChip && this.registerSubscriptions();
  }

  private importChipOnClick(args: EventData["ImportChip"]): void {
    pubsub.publish("AddCircuitChipToToolbar", {
      name: args.chipName,
      blueprint: args.blueprint,
    });
  }

  private spawnChipOnButtonClick(args: EventData["SpawnChip"]): void {
    let chip: Chip;

    switch (args.kind) {
      case "core":
        chip = this.circuitBoard.createCoreChip(args.name, false);
        break;
      case "circuit":
        chip =
          this.circuitBoard.blueprintService.createCircuitChipFromBlueprint(
            args.name,
            args.blueprint,
            args.color
          );
    }
    this.circuitBoard.setMode({ mode: Mode.SpawnChip, deps: { chip } });
  }

  private registerSubscriptions() {
    pubsub.subscribe("SaveCircuit", eventData =>
      this.circuitBoard.blueprintService.saveCircuit(eventData)
    );
    pubsub.subscribe("SpawnChip", eventData =>
      this.spawnChipOnButtonClick(eventData)
    );
    pubsub.subscribe("ImportChip", eventData =>
      this.importChipOnClick(eventData)
    );
  }
}
