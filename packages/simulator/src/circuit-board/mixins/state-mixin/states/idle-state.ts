import p5 from "p5";
import { pubsub } from "@circuit-sim/pubsub";
import { IOChip, IOSlider, Chip } from "../../../../chips";
import { Pin } from "../../../../pin";
import { AbstractState } from "./abstract-state";
import {
  ICircuitBoard,
  MouseInput,
  State,
} from "../../../circuit-board.interface";

export class IdleState extends AbstractState {
  constructor(p: p5, circuitBoard: ICircuitBoard) {
    super(p, circuitBoard);
  }

  public interact(mouseInput: MouseInput): void {
    const entity = this.circuitBoard.getMouseOverEntity(
      this.circuitBoard.entities
    );

    switch (mouseInput) {
      // TODO: split to individual handlers
      case MouseInput.Click:
        if (entity instanceof Pin) {
          if (entity.isInput) {
            pubsub.publish("Notification", {
              text: "Wires can only start from an output pin",
            });
            return;
          }
          this.circuitBoard.setState({
            state: State.Wiring,
            props: { startPin: entity },
          });
        } else if (entity instanceof IOChip) {
          entity.mouseClicked();
        } else if (entity instanceof IOSlider) {
          // TODO: Show update pin name dialog
        }
        break;

      case MouseInput.Drag:
        if (entity instanceof Chip) {
          this.circuitBoard.setState({
            state: State.Reposition,
            props: { chip: entity },
          });
        } else if (entity instanceof IOSlider) {
          this.circuitBoard.setState({
            state: State.Reposition,
            props: { chip: entity.chip },
          });
        }
        break;

      case MouseInput.Move:
        this.circuitBoard.isMouseOverIOChipPanel("input") &&
          this.circuitBoard.setState({
            state: State.SpawnIOChip,
            props: { kind: "input" },
          });

        this.circuitBoard.isMouseOverIOChipPanel("output") &&
          this.circuitBoard.setState({
            state: State.SpawnIOChip,
            props: { kind: "output" },
          });
    }
  }

  public dispose(): void {}
}
