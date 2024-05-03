import { IOChip, IOSlider } from "../../chip";
import type { Circuit } from "../circuit";
import { Interaction } from "../circuit.interface";
import { AbstractController } from "./abstract-controller";

export class IOChipSpawnController extends AbstractController {
  private ghostIOChip?: IOChip;

  constructor(p: p5, circuit: Circuit) {
    super(p, circuit);
  }

  public setGhostIOChip(iOChip: IOChip) {
    this.ghostIOChip = iOChip;
  }

  public clear(): void {
    this.ghostIOChip = undefined;
  }

  public handle(interaction: Interaction) {
    const entity = this.circuit.getMouseOverEntity();

    switch (interaction) {
      case Interaction.Click:
        this.ghostIOChip && this.circuit.spawnIOChip(this.ghostIOChip);
        break;

      case Interaction.Move:
        if (
          entity instanceof IOSlider ||
          (!this.circuit.isMouseOverInputChipPanel() &&
            !this.circuit.isMouseOverOutputChipPanel())
        ) {
          this.circuit.setIdleMode();
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
