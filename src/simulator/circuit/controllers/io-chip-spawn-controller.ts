import { IOChip, IOSlider } from "../../chips";
import type { Circuit } from "../circuit";
import { Mode } from "../circuit.interface";
import { AbstractController } from "./abstract-controller";
import { Interaction } from "./abstract-controller.interface";

export class IOChipSpawnController extends AbstractController {
  private ghostIOChip?: IOChip;

  constructor(p: p5, circuit: Circuit) {
    super(p, circuit);
  }

  public setGhostIOChip(iOChip: IOChip) {
    this.ghostIOChip = iOChip;
  }

  public stop(): void {
    this.ghostIOChip = undefined;
  }

  public start(interaction: Interaction) {
    const entity = this.circuit.renderer.getMouseOverEntity(
      this.circuit.entities
    );

    switch (interaction) {
      case Interaction.Click:
        this.ghostIOChip && this.circuit.spawnIOChip(this.ghostIOChip);
        break;

      case Interaction.Move:
        if (
          entity instanceof IOSlider ||
          (!this.circuit.renderer.isMouseOverIOChipPanel("input") &&
            !this.circuit.renderer.isMouseOverIOChipPanel("output"))
        ) {
          this.circuit.setMode({ mode: Mode.Idle });
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
