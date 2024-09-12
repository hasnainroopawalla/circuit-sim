import p5 from "p5";
import { IOChip, Chip } from "../../../chips";
import { AbstractState } from "./abstract-state";
import { ICircuitBoard } from "../../circuit-board-mixin";
import { State } from "../state-manager-mixin";
import { Interaction } from "../mouse-input-mixin";

export class RepositionState extends AbstractState {
  private chip?: Chip | IOChip;

  constructor(p: p5, circuitBoard: ICircuitBoard) {
    super(p, circuitBoard);
  }

  public setup(chip: Chip | IOChip): void {
    this.chip = chip;
  }

  public start(interaction: Interaction) {
    switch (interaction) {
      case Interaction.Drag:
        if (this.chip instanceof Chip && this.circuitBoard.isMouseOver()) {
          this.chip.mouseDragged();
        } else if (this.chip instanceof IOChip) {
          this.chip.mouseDragged();
        } else {
          this.circuitBoard.setState({ state: State.Idle });
        }
        break;
    }
  }

  public stop(): void {
    this.chip = undefined;
  }
}
