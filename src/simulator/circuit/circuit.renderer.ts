import {
  type Position,
  type Size,
  AbstractRenderer,
} from "../api/abstract-renderer";
import { config } from "./circuit.config";

export class CircuitRenderer extends AbstractRenderer {
  constructor(p: p5, position: Position, size: Size) {
    super(p, position, size);
  }

  public isMouseOver() {
    return (
      this.p.mouseX >= this.position.x &&
      this.p.mouseX <= this.position.x + this.size.w &&
      this.p.mouseY >= this.position.y &&
      this.p.mouseY <= this.position.y + this.size.h
    );
  }

  public isMouseOverIOChipPanel(kind: "input" | "output"): boolean {
    return kind === "input"
      ? this.p.mouseX >= 0 &&
          this.p.mouseX <= config.widthScale / 2 &&
          this.p.mouseY >= this.position.y &&
          this.p.mouseY <= this.position.y + this.size.h
      : // !this.isMouseOverlapping(this.inputs)
        this.p.mouseX >=
          this.position.x + this.size.w + config.widthScale / 2 &&
          this.p.mouseY >= this.position.y &&
          this.p.mouseY <= this.position.y + this.size.h;
        // !this.isMouseOverlapping(this.inputs)
  }

  public mouseDragged(): void {}

  public render() {
    this.p.push();
    this.p.stroke(config.strokeColor);
    this.p.strokeWeight(2);
    this.p.fill(config.background);
    this.p.rect(this.position.x, this.position.y, this.size.w, this.size.h);
    // slider section
    this.p.strokeWeight(0);
    this.p.fill(config.sliderSectionColor);
    this.p.rect(0, this.position.y, config.widthScale / 2, this.size.h);
    this.p.rect(
      this.position.x + this.size.w + config.widthScale / 2,
      this.position.y,
      config.widthScale / 2,
      this.size.h
    );
    this.p.pop();
  }
}
