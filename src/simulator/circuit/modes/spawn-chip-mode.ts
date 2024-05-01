import { Chip } from "../../chip";
// TODO: implement Circuit interface and import from there
import type { Circuit } from "../circuit";
import { Interaction } from "../circuit.interface";

export class ChipSpawner {
  p: p5;
  private ghostChips: Chip[];
  private circuit: Circuit

  constructor(p: p5, circuit: Circuit) {
    this.p = p;
    this.circuit = circuit
    this.ghostChips = [];
  }

  public clear() {
    this.ghostChips = [];
  }

  public createGhostChip(chip: Chip): void {
    this.ghostChips.push(chip);
  }

  public handle(interaction: Interaction) {
    switch (interaction) {
      case Interaction.Click:
        if (this.circuit.isMouseOver()) {
          this.circuit.spawnChips(this.ghostChips);
          this.circuit.setIdleMode()
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
