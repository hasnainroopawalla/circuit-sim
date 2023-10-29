import { config } from "../../../config";
import { config as renderConfig } from "../config";
import { Position } from "../../shared.interface";

export class PinRenderer {
  public static render(
    p: p5,
    name: string,
    position: Position,
    size: number,
    isMouseOver: boolean,
    isInput: boolean,
    isGhost: boolean
  ): void {
    p.stroke(config.component.circuit.background);
    p.strokeWeight(config.component.pin.strokeWeight);

    if (isMouseOver) {
      p.textSize(12);
      const textWidth = p.textWidth(name) + 5;

      const rect = {
        x: isInput ? position.x - size - textWidth : position.x + size / 2 + 4,
        y: position.y - size + 6,
        w: textWidth + 5,
        h: 18,
      };
      p.fill(config.component.pin.color);
      p.rect(rect.x, rect.y, rect.w, rect.h);
      p.noStroke();
      p.fill("white");
      p.textAlign(p.CENTER);
      p.text(name, rect.x + rect.w / 2, rect.y + rect.h / 2 + 4);
    } else {
      isGhost
        ? p.fill(renderConfig.ghostEntityColor)
        : p.fill(config.component.pin.color);
    }
    p.circle(position.x, position.y, size);
  }
}
