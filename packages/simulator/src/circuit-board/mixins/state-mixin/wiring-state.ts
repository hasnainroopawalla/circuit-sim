import p5 from "p5";
import { AbstractState } from "./abstract-state";
import type { ICircuitBoard } from "../../circuit-board.interface";
import { Interaction } from "../mouse-input-mixin";
import { State } from "./state-mixin";
import { wireConfig, WireMarker } from "../../../wire";
import { Pin } from "../../../pin";
import { computeReferencePoint } from "./state-mixin.utils";

export class WiringState extends AbstractState {
  private markers: WireMarker[];
  private startPin?: Pin;

  constructor(p: p5, circuitBoard: ICircuitBoard) {
    super(p, circuitBoard);
    this.markers = [];
  }

  public render(): void {
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
      this.startPin.renderer.position.x,
      this.startPin.renderer.position.y,
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

  public setup(pin: Pin): void {
    this.startPin = pin;
  }

  public start(interaction: Interaction) {
    const entity = this.circuitBoard.getMouseOverEntity(
      this.circuitBoard.entities
    );

    switch (interaction) {
      case Interaction.Click:
        // TODO: Improve logic
        if (this.circuitBoard.currentState === State.Wiring && this.startPin) {
          if (entity instanceof Pin) {
            // A wire is not allowed to start and end on the same chip
            if (this.startPin.chip !== entity.chip) {
              this.circuitBoard.spawnWire(this.startPin, entity, this.markers);
              this.circuitBoard.setState({ state: State.Idle });
            }
          } else {
            this.addWireMarker();
          }
        }
        break;

      case Interaction.DoubleClick:
        this.circuitBoard.setState({ state: State.Idle });
        break;
    }
  }

  public stop(): void {
    this.startPin = undefined;
    this.markers = [];
  }

  private addWireMarker(): void {
    const waypoint = {
      x: this.p.mouseX,
      y: this.p.mouseY,
    };
    this.markers.push({
      waypoint,
      referencePoint: computeReferencePoint(
        waypoint,
        // handle the initial scenario when there are no wire markers
        this.markers.length === 0 && this.startPin
          ? this.startPin.renderer.position
          : this.markers[this.markers.length - 1].waypoint
      ),
    });
  }
}
