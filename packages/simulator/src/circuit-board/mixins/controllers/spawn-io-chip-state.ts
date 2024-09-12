import p5 from "p5";
import { IOChip, IOSlider } from "../../../chips";
import { AbstractState } from "./abstract-state";
import { ICircuitBoard } from "../../circuit-board-mixin";
import { Interaction } from "../mouse-input-mixin";
import { State } from "../state-manager-mixin";

export class SpawnIOChipState extends AbstractState {
  private ghostIOChip?: IOChip;

  constructor(p: p5, circuitBoard: ICircuitBoard) {
    super(p, circuitBoard);
  }

  public setup(iOChip: IOChip) {
    this.ghostIOChip = iOChip;
  }

  public start(interaction: Interaction) {
    const entity = this.circuitBoard.getMouseOverEntity(
      this.circuitBoard.entities
    );

    switch (interaction) {
      case Interaction.Click:
        this.ghostIOChip && this.circuitBoard.spawnIOChip(this.ghostIOChip);
        break;

      case Interaction.Move:
        if (
          entity instanceof IOSlider ||
          (!this.circuitBoard.isMouseOverIOChipPanel("input") &&
            !this.circuitBoard.isMouseOverIOChipPanel("output"))
        ) {
          this.circuitBoard.setState({ state: State.Idle });
        }
        break;
    }
  }

  public stop(): void {
    this.ghostIOChip = undefined;
  }

  public render(): void {
    if (!this.ghostIOChip) {
      return;
    }
    this.ghostIOChip.mouseDragged();
    this.ghostIOChip.render();
  }
}
