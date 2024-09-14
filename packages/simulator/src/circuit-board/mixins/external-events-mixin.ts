import { BaseMixin } from "power-mixin";
import { ICircuitBoard, State } from "../circuit-board.interface";
import { EventData, pubsub } from "@circuit-sim/pubsub";
import { Chip } from "../../chips";

export type IExternalEventService = object;

class ExternalEventService implements IExternalEventService {
  private circuitBoard: ICircuitBoard;

  constructor(circuitBoard: ICircuitBoard) {
    this.circuitBoard = circuitBoard;

    if (!circuitBoard.isCircuitChip) {
      this.registerSubscriptions();
    }
  }

  private registerSubscriptions(): void {
    pubsub.subscribe("SaveCircuit", eventData =>
      this.circuitBoard.saveCircuit(eventData)
    );
    pubsub.subscribe("SpawnChip", eventData =>
      this.spawnChipOnButtonClick(eventData)
    );
    pubsub.subscribe("ImportChip", eventData =>
      this.importChipOnClick(eventData)
    );
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
        chip = this.circuitBoard.loadCircuit(
          args.name,
          args.blueprint,
          args.color
        );
    }
    this.circuitBoard.setState({ state: State.SpawnChip, props: { chip } });
  }
}

export class ExternalEventServiceMixin extends BaseMixin<
  ICircuitBoard,
  IExternalEventService
> {
  constructor() {
    super({
      methods: [],
      props: [],
      initMixin: circuitBoard => new ExternalEventService(circuitBoard),
    });
  }
}
