import { Chip } from "../../chip";

export class ChipSpawner {
  p: p5;
  ghostChips: Chip[];

  constructor(p5: p5) {
    this.p = p5;
    this.ghostChips = []
  }

  public createGhostChip(chip: Chip): void {
    this.ghostChips.push(chip);
  }

  public renderGhostChips(): void {
    for (let i = 0; i < this.ghostChips.length; i++) {
      const chip = this.ghostChips[i];
      chip.setPosition();
      chip.options.position = {
        x: this.p.mouseX - chip.options.size.w / 2,
        y:
          this.p.mouseY -
          chip.options.size.h / 2 -
          (i * chip.options.size.h) / 0.8, // Extra offset for spacing between chips
      };
      chip.render();
    }
  }
}
