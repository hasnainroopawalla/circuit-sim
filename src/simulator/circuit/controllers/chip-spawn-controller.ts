import { Chip } from "../../chips";
import type { Circuit } from "../circuit";
import { Mode } from "../circuit.interface";
import { AbstractController } from "./abstract-controller";
import { Interaction } from "./abstract-controller.interface";

// TODO: consider merging the logic with IOChipSpawn
export class ChipSpawnController extends AbstractController {
  private ghostChips: Chip[];

  constructor(p: p5, circuit: Circuit) {
    super(p, circuit);
    this.ghostChips = [];
  }

  public stop() {
    this.ghostChips = [];
  }

  public setGhostChip(chip: Chip): void {
    this.ghostChips.push(chip);
  }

  public start(interaction: Interaction) {
    switch (interaction) {
      case Interaction.Click:
        if (this.circuit.renderer.isMouseOver()) {
          this.spawnGhostChips();
          this.circuit.setMode({ mode: Mode.Idle });
        }
        break;
    }
  }

  public renderGhostChips(): void {
    for (let i = 0; i < this.ghostChips.length; i++) {
      const chip = this.ghostChips[i];

      chip.setPosition({
        x: this.p.mouseX - chip.renderer.size.w / 2,
        y:
          this.p.mouseY -
          chip.renderer.size.h / 2 -
          (i * chip.renderer.size.h) / 0.8, // Extra offset for spacing between chips
      });

      chip.render();
    }
  }

  private spawnGhostChips(): void {
    this.ghostChips.forEach((ghostChip) => this.circuit.spawnChip(ghostChip));
  }
}
