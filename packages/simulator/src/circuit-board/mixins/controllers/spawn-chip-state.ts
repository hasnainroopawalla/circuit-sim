import p5 from "p5";
import { Chip } from "../../../chips";
import { AbstractState } from "./abstract-state";
import { ICircuitBoard } from "../../circuit-board-mixin";
import { Interaction } from "../mouse-input-mixin";
import { State } from "../state-manager-mixin";

export class SpawnChipState extends AbstractState {
  private ghostChips: Chip[];

  constructor(p: p5, circuitBoard: ICircuitBoard) {
    super(p, circuitBoard);
    this.ghostChips = [];
  }

  public setup(chip: Chip): void {
    this.ghostChips.push(chip);
  }

  public start(interaction: Interaction) {
    switch (interaction) {
      case Interaction.Click:
        if (this.circuitBoard.isMouseOver()) {
          this.spawnGhostChips();
          this.circuitBoard.setState({ state: State.Idle });
        }
        break;
    }
  }

  public stop() {
    this.ghostChips = [];
  }

  public render(): void {
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
    this.ghostChips.forEach(ghostChip =>
      this.circuitBoard.spawnChip(ghostChip)
    );
  }
}
