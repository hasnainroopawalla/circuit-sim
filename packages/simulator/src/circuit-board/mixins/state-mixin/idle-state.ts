import p5 from "p5";
import { pubsub } from "@circuit-sim/pubsub";
import { IOChip, IOSlider, Chip } from "../../../chips";
import { Pin } from "../../../pin";
import { AbstractState } from "./abstract-state";
import type { ICircuitBoard } from "../../circuit-board.interface";
import { State } from "./state-mixin";
import { Interaction } from "../mouse-input-mixin";

export class IdleState extends AbstractState {
  constructor(p: p5, circuitBoard: ICircuitBoard) {
    super(p, circuitBoard);
  }

  public start(interaction: Interaction) {
    const entity = this.circuitBoard.getMouseOverEntity(
      this.circuitBoard.entities
    );

    switch (interaction) {
      case Interaction.Click:
        if (entity instanceof Pin) {
          if (entity.isInput) {
            pubsub.publish("Notification", {
              text: "Wires can only start from an output pin",
            });
            return;
          }
          this.circuitBoard.setState({
            state: State.Wiring,
            deps: { startPin: entity },
          });
        } else if (entity instanceof IOChip) {
          entity.mouseClicked();
        } else if (entity instanceof IOSlider) {
          // TODO: Show update pin name dialog
        }
        break;

      case Interaction.Drag:
        if (entity instanceof Chip) {
          this.circuitBoard.setState({
            state: State.Reposition,
            deps: { chip: entity },
          });
        } else if (entity instanceof IOSlider) {
          this.circuitBoard.setState({
            state: State.Reposition,
            deps: { chip: entity.chip },
          });
        }
        break;

      case Interaction.Move:
        this.circuitBoard.isMouseOverIOChipPanel("input") &&
          this.circuitBoard.setState({
            state: State.SpawnIOChip,
            deps: { kind: "input" },
          });

        this.circuitBoard.isMouseOverIOChipPanel("output") &&
          this.circuitBoard.setState({
            state: State.SpawnIOChip,
            deps: { kind: "output" },
          });
    }
  }

  public stop(): void {}

  public render(): void {}
}
