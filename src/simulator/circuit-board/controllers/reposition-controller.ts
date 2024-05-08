import { Chip, IOChip } from "../../chips";
import type { CircuitBoard } from "../circuit-board";
import { Mode } from "../circuit-board.interface";
import { AbstractController } from "./abstract-controller";
import { Interaction } from "./abstract-controller.interface";

export class RepositionController extends AbstractController {
  private chip?: Chip | IOChip;

  constructor(p: p5, circuitBoard: CircuitBoard) {
    super(p, circuitBoard);
  }

  public setChip(chip: Chip | IOChip): void {
    this.chip = chip;
  }

  public stop(): void {
    this.chip = undefined;
  }

  public start(interaction: Interaction) {
    switch (interaction) {
      case Interaction.Drag:
        if (
          this.chip instanceof Chip &&
          this.circuitBoard.renderer.isMouseOver()
        ) {
          this.chip.mouseDragged();
        } else if (this.chip instanceof IOChip) {
          this.chip.mouseDragged();
        } else {
          this.circuitBoard.setMode({ mode: Mode.Idle });
        }
        break;
    }
  }
}
