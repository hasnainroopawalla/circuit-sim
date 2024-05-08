import { emitter } from "@circuit-sim/events";
import { Chip, IOChip, IOSlider } from "../../chips";
import { Pin } from "../../pin";
import type { CircuitBoard } from "../circuit-board";
import { Mode } from "../circuit-board.interface";
import { AbstractController } from "./abstract-controller";
import { Interaction } from "./abstract-controller.interface";

export class IdleModeController extends AbstractController {
  constructor(p: p5, circuitBoard: CircuitBoard) {
    super(p, circuitBoard);
  }

  public stop(): void {}

  public start(interaction: Interaction) {
    const entity = this.circuitBoard.renderer.getMouseOverEntity(
      this.circuitBoard.entities
    );

    switch (interaction) {
      case Interaction.Click:
        if (entity instanceof Pin) {
          if (entity.isInput) {
            emitter.emit("Notification", {
              text: "Wires can only start from an output pin",
            });
          }
          this.circuitBoard.setMode({
            mode: Mode.Wiring,
            deps: { startPin: entity },
          });
        } else if (entity instanceof IOChip) {
          entity.mouseClicked();
        } else if (entity instanceof IOSlider) {
          // TODO: Show update pin name dialog
        }
        break;

      case Interaction.Drag:
        if (entity instanceof Chip) {
          this.circuitBoard.setMode({
            mode: Mode.Reposition,
            deps: { chip: entity },
          });
        } else if (entity instanceof IOSlider) {
          this.circuitBoard.setMode({
            mode: Mode.Reposition,
            deps: { chip: entity.chip },
          });
        }
        break;

      case Interaction.Move:
        this.circuitBoard.renderer.isMouseOverIOChipPanel("input") &&
          this.circuitBoard.setMode({
            mode: Mode.SpawnIOChipHover,
            deps: { kind: "input" },
          });

        this.circuitBoard.renderer.isMouseOverIOChipPanel("output") &&
          this.circuitBoard.setMode({
            mode: Mode.SpawnIOChipHover,
            deps: { kind: "output" },
          });
    }
  }
}
