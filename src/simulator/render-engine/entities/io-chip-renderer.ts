import { config } from "../../../config";
import { config as renderConfig } from "../config";
import { Pin } from "../../pin";
import { Position, State } from "../../shared.interface";
import { Utils } from "../utils";

export class IOChipRenderer {
  private static renderChip(
    p: p5,
    pin: Pin,
    position: Position,
    size: number,
    isGhost: boolean
  ): void {
    if (isGhost) {
      p.strokeWeight(0);
      p.fill(renderConfig.ghostEntityColor); // TODO: move to config
    } else {
      p.strokeWeight(config.component.iOChip.strokeWeight);
      p.fill(
        pin.state === State.Off
          ? config.component.iOChip.color.stateOff
          : config.component.iOChip.color.stateOn
      );
    }
    p.circle(position.x, position.y, size);
  }

  private static renderInnerWire(
    p: p5,
    position: Position,
    size: number,
    isInput: boolean,
    isGhost: boolean
  ): void {
    isGhost
      ? p.stroke(renderConfig.ghostEntityColor)
      : p.stroke(config.component.iOChip.innerWire.color);
    p.strokeWeight(config.component.iOChip.innerWire.strokeWeight);
    p.line(
      isInput ? position.x + size / 2 : position.x - size / 2,
      position.y,
      isInput ? position.x + size : position.x - size,
      position.y
    );
  }

  private static renderPin(
    pin: Pin,
    position: Position,
    size: number,
    isInput: boolean
  ): void {
    const pinPosition = Utils.iOPinPosition(position, size, isInput);
    pin.setPosition(pinPosition);
    pin.render();
  }

  public static render(
    p: p5,
    pin: Pin,
    position: Position,
    size: number,
    isInput: boolean,
    isGhost: boolean
  ): void {
    this.renderInnerWire(p, position, size, isInput, isGhost);
    this.renderChip(p, pin, position, size, isGhost);
    this.renderPin(pin, position, size, isInput);
  }
}
