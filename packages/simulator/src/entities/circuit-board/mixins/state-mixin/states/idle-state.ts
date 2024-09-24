import p5 from "p5";
import { pubsub } from "@circuit-sim/pubsub";
import { AbstractState } from "./abstract-state";
import {
  ICircuitBoard,
  MouseInput,
  State,
} from "../../../circuit-board.interface";
import { IOChip, IOSlider, Chip } from "../../../../chips";
import { Pin } from "../../../../pin";
import { Entity } from "../../../../entity.interface";

export class IdleState extends AbstractState {
  constructor(p: p5, circuitBoard: ICircuitBoard) {
    super(p, circuitBoard);
  }

  public interact(mouseInput: MouseInput): void {
    const entity = this.circuitBoard.getMouseOverEntity(
      this.circuitBoard.entities
    );

    switch (mouseInput) {
      case MouseInput.Click:
        this.handleMouseClick(entity);
        break;

      case MouseInput.Drag:
        this.handleMouseDrag(entity);
        break;

      case MouseInput.Move:
        this.handleMouseMove(entity);
        break;
    }
  }

  public dispose(): void {}

  private handleMouseClick(entity: Entity | undefined): void {
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
    }
  }

  private handleMouseMove(entity: Entity | undefined): void {
    if (entity instanceof IOSlider) {
      return;
    }

    if (this.circuitBoard.isMouseOverIOChipPanel("input")) {
      this.circuitBoard.setState({
        state: State.SpawnIOChip,
        props: { kind: "input" },
      });
    }

    if (this.circuitBoard.isMouseOverIOChipPanel("output")) {
      this.circuitBoard.setState({
        state: State.SpawnIOChip,
        props: { kind: "output" },
      });
    }
  }

  private handleMouseDrag(entity: Entity | undefined): void {
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
  }
}
