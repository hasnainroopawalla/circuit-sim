import { Chip, IOChip } from "../../chip";
import type { Circuit } from "../circuit";
import { Interaction } from "../circuit.interface";
import { AbstractController } from "./abstract-controller";

export class RepositionController extends AbstractController {
  private chip?: Chip | IOChip;

  constructor(p: p5, circuit: Circuit) {
    super(p, circuit);
  }

  public setChip(chip: Chip | IOChip): void {
    this.chip = chip;
  }

  public clear(): void {
    this.chip = undefined;
  }

  public handle(interaction: Interaction) {
    switch (interaction) {
      case Interaction.Drag:
        if (this.chip instanceof Chip && this.circuit.isMouseOver()) {
          this.chip.mouseDragged();
        } else if (this.chip instanceof IOChip) {
          this.chip.mouseDragged();
        } else {
          this.circuit.setIdleMode();
        }
        break;
    }
  }
}