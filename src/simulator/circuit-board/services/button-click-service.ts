import { Chip } from "../../chips";
import type { CircuitBoard } from "../circuit-board";
import { Mode } from "../circuit-board.interface";
import { EventData, emitter } from "@circuit-sim/events";

export class ButtonClickService {
  p: p5;
  circuitBoard: CircuitBoard;

  constructor(p: p5, circuitBoard: CircuitBoard) {
    this.p = p;
    this.circuitBoard = circuitBoard;
    !this.circuitBoard.isCircuitChip && this.registerSubscriptions();
  }

  // TODO: color-generator to service
  // TODO: web as a package
  // TODO: simulator as a package
  // TODO: src->packages
  private importChipOnClick(args: EventData["ImportChip"]): void {
    emitter.emit("AddCircuitChipToToolbar", {
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
            "main"
          );
    }
    this.circuitBoard.setMode({ mode: Mode.SpawnChip, deps: { chip } });
  }

  private registerSubscriptions() {
    emitter.on("SaveCircuit", (eventData) =>
      this.circuitBoard.blueprintService.saveCircuit(eventData)
    );
    emitter.on("SpawnChip", (eventData) =>
      this.spawnChipOnButtonClick(eventData)
    );
    emitter.on("ImportChip", (eventData) => this.importChipOnClick(eventData));
  }
}
