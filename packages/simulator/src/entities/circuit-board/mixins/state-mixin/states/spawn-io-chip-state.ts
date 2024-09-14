import p5 from "p5";
import { AbstractState } from "./abstract-state";
import {
  State,
  ICircuitBoard,
  MouseInput,
} from "../../../circuit-board.interface";
import { IOChip, IOSlider } from "../../../../chips";
import { Entity } from "../../../../entity.interface";

export class SpawnIOChipState extends AbstractState {
  private ghostIOChip?: IOChip;

  constructor(p: p5, circuitBoard: ICircuitBoard) {
    super(p, circuitBoard);
  }

  public setup(iOChip: IOChip) {
    this.ghostIOChip = iOChip;
  }

  public interact(mouseInput: MouseInput) {
    const entity = this.circuitBoard.getMouseOverEntity(
      this.circuitBoard.entities
    );

    switch (mouseInput) {
      case MouseInput.Click:
        this.handleMouseClick();
        break;

      case MouseInput.Move:
        this.handleMouseMove(entity);
        break;
    }
  }

  public dispose(): void {
    this.ghostIOChip = undefined;
  }

  public render(): void {
    if (!this.ghostIOChip) {
      return;
    }
    this.ghostIOChip.mouseDragged();
    this.ghostIOChip.render();
  }

  private handleMouseClick(): void {
    this.ghostIOChip && this.circuitBoard.spawnIOChip(this.ghostIOChip);
  }

  private handleMouseMove(entity: Entity | undefined): void {
    if (
      entity instanceof IOSlider ||
      (!this.circuitBoard.isMouseOverIOChipPanel("input") &&
        !this.circuitBoard.isMouseOverIOChipPanel("output"))
    ) {
      this.circuitBoard.setState({ state: State.Idle });
    }
  }
}
