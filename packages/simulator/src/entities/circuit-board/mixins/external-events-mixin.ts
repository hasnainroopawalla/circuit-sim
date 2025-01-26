import { BaseMixin } from "ts-mxn";
import { ICircuitBoard, State } from "../circuit-board.interface";
import { EventData, pubsub } from "@circuit-sim/pubsub";
import { Chip } from "../../chips";

export type IExternalEventsService = object;

type IExternalEventsServiceArgs = {
  circuitBoard: ICircuitBoard;
};

class ExternalEventsService implements IExternalEventsService {
  private circuitBoard: ICircuitBoard;

  constructor(args: IExternalEventsServiceArgs) {
    this.circuitBoard = args.circuitBoard;

    if (!args.circuitBoard.isCircuitChip) {
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

type IExternalEventsMixinArgs = Omit<
  IExternalEventsServiceArgs,
  "circuitBoard"
>;

export class ExternalEventsMixin extends BaseMixin<
  ICircuitBoard,
  IExternalEventsService
> {
  constructor(args: IExternalEventsMixinArgs) {
    super({
      methods: [],
      props: [],
      initMixin: circuitBoard =>
        new ExternalEventsService({ circuitBoard, ...args }),
    });
  }
}
