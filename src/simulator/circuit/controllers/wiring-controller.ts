// TODO: move to controller-utils
import { CircuitHelper } from "../../helpers/circuit-helper";
import { Pin } from "../../pin";
// TODO: move all configs to single dir
import { WireMarker, config as wireConfig } from "../../wire";
import type { Circuit } from "../circuit";
import { Interaction, Mode } from "../circuit.interface";
import { AbstractController } from "./abstract-controller";

export class WiringController extends AbstractController {
  private markers: WireMarker[];
  private startPin?: Pin;

  constructor(p: p5, circuit: Circuit) {
    super(p, circuit);
    this.markers = [];
  }

  public renderGhostWire(): void {
    // TODO: duplicate render logic from wire.ts
    if (!this.startPin) {
      throw new Error("Wiring mode start pin not defined");
    }
    this.p.push();
    this.p.strokeWeight(wireConfig.strokeWeight);
    this.p.stroke(wireConfig.color.stateOff);
    this.p.noFill();

    // render initial line from startPin to either mouse position or first waypoint
    this.p.line(
      this.startPin.position.x,
      this.startPin.position.y,
      this.markers.length === 0
        ? this.p.mouseX
        : this.markers[0].referencePoint.x,
      this.markers.length === 0
        ? this.p.mouseY
        : this.markers[0].referencePoint.y
    );

    for (let i = 0; i < this.markers.length; i++) {
      const startPoint = this.markers[i].referencePoint;
      const controlPoint = this.markers[i].waypoint;

      // The end point of the wire should be the current mouse position
      const endPoint =
        i === this.markers.length - 1
          ? { x: this.p.mouseX, y: this.p.mouseY }
          : this.markers[i + 1].referencePoint;

      this.p.bezier(
        startPoint.x,
        startPoint.y,
        controlPoint.x,
        controlPoint.y,
        controlPoint.x,
        controlPoint.y,
        endPoint.x,
        endPoint.y
      );
    }
    this.p.pop();
  }

  public setStartPin(pin: Pin): void {
    this.startPin = pin;
  }

  public clear(): void {
    this.startPin = undefined;
    this.markers = [];
  }

  public handle(interaction: Interaction) {
    const entity = this.circuit.renderer.getMouseOverEntity(
      this.circuit.entities
    );

    switch (interaction) {
      case Interaction.Click:
        // TODO: Improve logic
        if (this.circuit.mode === Mode.Wiring && this.startPin) {
          if (entity instanceof Pin) {
            // A wire is not allowed to start and end on the same chip
            if (this.startPin.chip !== entity.chip) {
              this.circuit.spawnWire(this.startPin, entity, this.markers);
              this.circuit.setMode({ mode: Mode.Idle });
            }
          } else {
            this.addWireMarker();
          }
        }
        break;

      case Interaction.DoubleClick:
        this.circuit.setMode({ mode: Mode.Idle });
        break;
    }
  }

  private addWireMarker(): void {
    const waypoint = {
      x: this.p.mouseX,
      y: this.p.mouseY,
    };
    this.markers.push({
      waypoint,
      referencePoint: CircuitHelper.computeReferencePoint(
        waypoint,
        // handle the initial scenario when there are no wire markers
        this.markers.length === 0 && this.startPin
          ? this.startPin.position
          : this.markers[this.markers.length - 1].waypoint
      ),
    });
  }
}
