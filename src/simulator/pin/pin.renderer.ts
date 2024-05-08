import { AbstractRenderer, Position, Size } from "../common";
import { config } from "../config";
import type { Pin } from "./pin";
import { pinConfig } from "./pin.config";

export class PinRenderer extends AbstractRenderer<Size<"circle">> {
  pin: Pin;

  constructor(p: p5, pin: Pin) {
    super(p, { x: 0, y: 0 }, { d: pinConfig.size });
    this.pin = pin;
  }

  public isMouseOver(): boolean {
    return (
      this.p.dist(
        this.p.mouseX,
        this.p.mouseY,
        this.position.x,
        this.position.y
      ) <=
      this.size.d / 2
    );
  }

  public render(): void {
    this.p.push();
    this.p.stroke(pinConfig.strokeColor);
    this.p.strokeWeight(pinConfig.strokeWeight);

    if (this.isMouseOver()) {
      this.p.textSize(12);
      const textWidth = this.p.textWidth(this.pin.name) + 5;

      const rect = {
        x: this.pin.isInput
          ? this.position.x - this.size.d - textWidth
          : this.position.x + this.size.d / 2 + 4,
        y: this.position.y - this.size.d + 6,
        w: textWidth + 5,
        h: 18,
      };
      this.p.noStroke();
      this.p.fill(pinConfig.color);
      this.p.rect(rect.x, rect.y, rect.w, rect.h);
      this.p.fill("white");
      this.p.textAlign(this.p.CENTER);
      this.p.text(this.pin.name, rect.x + rect.w / 2, rect.y + rect.h / 2 + 4);
    } else {
      this.pin.isGhost
        ? this.p.fill(config.ghostEntityColor)
        : this.p.fill(pinConfig.color);
    }
    this.p.circle(this.position.x, this.position.y, this.size.d);
    this.p.pop();
  }

  public setPosition(position: Position): void {
    this.position = position;
  }
}
