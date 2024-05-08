import p5 from "p5";
import { IOChip, IOSlider } from "../../chips";
import type { CircuitBoard } from "../circuit-board";
import { Mode } from "../circuit-board.interface";
import { AbstractController } from "./abstract-controller";
import { Interaction } from "./abstract-controller.interface";

export class IOChipSpawnController extends AbstractController {
  private ghostIOChip?: IOChip;

  constructor(p: p5, circuitBoard: CircuitBoard) {
    super(p, circuitBoard);
  }

  public setGhostIOChip(iOChip: IOChip) {
    this.ghostIOChip = iOChip;
  }

  public stop(): void {
    this.ghostIOChip = undefined;
  }

  public start(interaction: Interaction) {
    const entity = this.circuitBoard.renderer.getMouseOverEntity(
      this.circuitBoard.entities
    );

    switch (interaction) {
      case Interaction.Click:
        this.ghostIOChip && this.circuitBoard.spawnIOChip(this.ghostIOChip);
        break;

      case Interaction.Move:
        if (
          entity instanceof IOSlider ||
          (!this.circuitBoard.renderer.isMouseOverIOChipPanel("input") &&
            !this.circuitBoard.renderer.isMouseOverIOChipPanel("output"))
        ) {
          this.circuitBoard.setMode({ mode: Mode.Idle });
        }
        break;
    }
  }

  public renderGhostIOChip(): void {
    if (!this.ghostIOChip) {
      return;
    }
    this.ghostIOChip.mouseDragged();
    this.ghostIOChip.render();
  }
}
