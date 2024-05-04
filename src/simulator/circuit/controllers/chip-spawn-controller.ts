import { Chip } from "../../chip";
import type { Circuit } from "../circuit";
import { Interaction, Mode } from "../circuit.interface";
import { AbstractController } from "./abstract-controller";

// TODO: consider merging the logic with IOChipSpawn
export class ChipSpawnController extends AbstractController {
  private ghostChips: Chip[];

  constructor(p: p5, circuit: Circuit) {
    super(p, circuit);
    this.ghostChips = [];
  }

  private spawnGhostChips(): void {
    this.ghostChips.forEach((ghostChip) => this.circuit.spawnChip(ghostChip));
  }

  public clear() {
    this.ghostChips = [];
  }

  public setGhostChip(chip: Chip): void {
    this.ghostChips.push(chip);
  }

  public handle(interaction: Interaction) {
    switch (interaction) {
      case Interaction.Click:
        if (this.circuit.isMouseOver()) {
          this.spawnGhostChips();
          this.circuit.setMode({mode: Mode.Idle})
        }
        break;
    }
  }

  public renderGhostChips(): void {
    for (let i = 0; i < this.ghostChips.length; i++) {
      const chip = this.ghostChips[i];
      // TODO: dont access the chip renderer directly
      const position = {
        x: this.p.mouseX - chip.renderer.size.w / 2,
        y:
          this.p.mouseY -
          chip.renderer.size.h / 2 -
          (i * chip.renderer.size.h) / 0.8, // Extra offset for spacing between chips
      };

      chip.setPosition(position);
      chip.render();
    }
  }
}
