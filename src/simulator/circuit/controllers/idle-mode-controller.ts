import { EmitterHelper } from "../../../event-service";
import { Chip, IOChip, IOSlider } from "../../chips";
import { Pin } from "../../pin";
import type { Circuit } from "../circuit";
import { Interaction, Mode } from "../circuit.interface";
import { AbstractController } from "./abstract-controller";

export class IdleModeController extends AbstractController {
  constructor(p: p5, circuit: Circuit) {
    super(p, circuit);
  }

  public clear(): void {}

  public handle(interaction: Interaction) {
    const entity = this.circuit.renderer.getMouseOverEntity(
      this.circuit.entities
    );

    switch (interaction) {
      case Interaction.Click:
        if (entity instanceof Pin) {
          if (entity.isInput) {
            return EmitterHelper.notification(
              "Wires can only start from an output pin"
            );
          }
          this.circuit.setMode({
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
          this.circuit.setMode({
            mode: Mode.Reposition,
            deps: { chip: entity },
          });
        } else if (entity instanceof IOSlider) {
          this.circuit.setMode({
            mode: Mode.Reposition,
            deps: { chip: entity.chip },
          });
        }
        break;

      case Interaction.Move:
        this.circuit.renderer.isMouseOverIOChipPanel("input") &&
          this.circuit.setMode({
            mode: Mode.SpawnIOChipHover,
            deps: { kind: "input" },
          });

        this.circuit.renderer.isMouseOverIOChipPanel("output") &&
          this.circuit.setMode({
            mode: Mode.SpawnIOChipHover,
            deps: { kind: "output" },
          });
    }
  }
}
