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

  // TODO: can be merged with coreChipButtonOnClick
  private spawnCircuitChipButtonOnClick(
    args: EventData["SpawnCircuitChip"]
  ): void {
    const circuitChip =
      this.circuitBoard.blueprintService.createCircuitChipFromBlueprint(
        args.name,
        args.blueprint,
        "main"
      );

    this.circuitBoard.setMode({
      mode: Mode.SpawnChip,
      deps: { chip: circuitChip },
    });
  }

  private spawnCoreChipButtonOnClick(args: EventData["SpawnCoreChip"]): void {
    const chip = this.circuitBoard.createCoreChip(args.coreChip, false);
    this.circuitBoard.setMode({ mode: Mode.SpawnChip, deps: { chip } });
  }

  private importChipOnClick(args: EventData["ImportChip"]): void {
    emitter.emit("AddCircuitChipToToolbar", {
      name: args.chipName,
      blueprint: args.blueprint,
    });
  }

  private registerSubscriptions() {
    emitter.on("SaveCircuit", (eventData) =>
      this.circuitBoard.blueprintService.saveCircuit(eventData)
    );
    emitter.on("SpawnCoreChip", (eventData) =>
      this.spawnCoreChipButtonOnClick(eventData)
    );
    emitter.on("SpawnCircuitChip", (eventData) =>
      this.spawnCircuitChipButtonOnClick(eventData)
    );
    emitter.on("ImportChip", (eventData) => this.importChipOnClick(eventData));
  }
}
